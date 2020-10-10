package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.{XSDebug}

class StorePipe extends DCacheModule
{
  val io = IO(new DCacheBundle{
    val lsu        = Flipped(new DCacheStoreIO)
    val data_read  = DecoupledIO(new L1DataReadReq)
    val data_resp  = Input(Vec(nWays, Vec(blockRows, Bits(encRowBits.W))))
    val data_write = DecoupledIO(new L1DataWriteReq)
    val meta_read  = DecoupledIO(new L1MetaReadReq)
    val meta_resp  = Input(Vec(nWays, new L1Metadata))
    val inflight_req_idxes       = Output(Vec(3, Valid(UInt())))
    val inflight_req_block_addrs = Output(Vec(3, Valid(UInt())))
  })


  // LSU requests
  val s1_wait_data_read = Wire(Bool())
  val continue_waiting = Wire(Bool())
  io.lsu.req.ready := io.meta_read.ready && !s1_wait_data_read
  io.meta_read.valid := io.lsu.req.valid && !s1_wait_data_read

  val meta_read = io.meta_read.bits

  // Tag read for new requests
  meta_read.idx    := get_idx(io.lsu.req.bits.addr)
  meta_read.way_en := ~0.U(nWays.W)
  meta_read.tag    := DontCare

  // Pipeline
  // stage 0
  val s0_valid = io.lsu.req.fire()
  val s0_req = io.lsu.req.bits

  assert(!(s0_valid && s0_req.cmd =/= MemoryOpConstants.M_XWR), "StorePipe only accepts store req")

  dump_pipeline_reqs("StorePipe s0", s0_valid, s0_req)

  // stage 1
  val s1_req = Reg(new DCacheLineReq)
  val s1_valid = RegInit(init = false.B)
  when (!s1_wait_data_read) {
    s1_valid := s0_valid
    s1_req := s0_req
  }
  val s1_addr = s1_req.addr

  dump_pipeline_reqs("StorePipe s1", s1_valid, s1_req)

  val meta_resp_reg = Reg(Vec(nWays, new L1Metadata))
  val meta_resp = Mux(continue_waiting, meta_resp_reg, io.meta_resp)
  meta_resp_reg := meta_resp

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta_resp(w).tag === (get_tag(s1_addr))).asUInt
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta_resp(w).coh.isValid()).asUInt
  val s1_tag_match     = s1_tag_match_way.orR
  val s1_hit_state     = Mux1H(s1_tag_match_way, wayMap((w: Int) => meta_resp(w).coh))
  val s1_has_permission = s1_hit_state.onAccess(s1_req.cmd)._1
  val s1_new_hit_state  = s1_hit_state.onAccess(s1_req.cmd)._3

  // we not only need permissions
  // we also require that state does not change on hit
  // thus we require new_hit_state === old_hit_state
  //
  // If state changes on hit,
  // we should treat it as not hit, and let mshr deal with it,
  // since we can not write meta data on the main pipeline.
  // It's possible that we had permission but state changes on hit:
  // eg: write to exclusive but clean block
  val s1_hit = s1_tag_match && s1_has_permission && s1_hit_state === s1_new_hit_state

  val data_read = io.data_read.bits
  io.data_read.valid := s1_valid && s1_hit
  // Data read for new requests
  data_read.addr   := s1_req.addr
  data_read.way_en := s1_tag_match_way
  // only needs to read the specific row
  data_read.rmask  := UIntToOH(get_row(s1_req.addr))

  s1_wait_data_read := io.data_read.valid && !io.data_read.ready
  continue_waiting := RegNext(s1_wait_data_read, init = false.B)

  val s1_info = p"tag match: $s1_tag_match hasPerm: $s1_has_permission" +
    p" hit state: $s1_hit_state new state: $s1_new_hit_state\n"
  // only dump these signals when they are actually valid
  dump_pipeline_valids("StorePipe s1", "s1_hit", s1_valid && s1_hit)

  // stage 2
  val s2_req   = RegNext(s1_req)
  val s2_valid = RegNext(s1_valid && !s1_wait_data_read, init = false.B)

  dump_pipeline_reqs("StorePipe s2", s2_valid, s2_req)

  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_hit = RegNext(s1_hit)
  val s2_nack = false.B

  val data_resp = io.data_resp
  val s2_data = Mux1H(s2_tag_match_way, data_resp)
  val s2_data_decoded = (0 until blockRows) map { r =>
    (0 until rowWords) map { w =>
      val data = s2_data(r)(encWordBits * (w + 1) - 1, encWordBits * w)
      val decoded = cacheParams.dataCode.decode(data)
      assert(!(s2_valid && s2_hit && !s2_nack && decoded.uncorrectable))
      decoded.corrected
    }
  }

  val wdata_merged = Wire(Vec(blockRows, UInt(encRowBits.W)))

  def mergePutData(old_data: UInt, new_data: UInt, wmask: UInt): UInt = {
    val full_wmask = FillInterleaved(8, wmask)
    ((~full_wmask & old_data) | (full_wmask & new_data))
  }

  // now, we do not deal with ECC
  for (i <- 0 until blockRows) {
    wdata_merged(i) := Cat((0 until rowWords).reverse map { w =>
      val old_data = s2_data_decoded(i)(w)
      val new_data = s2_req.data(rowBits * (i + 1) - 1, rowBits * i)(wordBits * (w + 1) - 1, wordBits * w)
      val wmask = s2_req.mask(rowBytes * (i + 1) - 1, rowBytes * i)(wordBytes * (w + 1) - 1, wordBytes * w)
      val wdata = mergePutData(old_data, new_data, wmask)
      val wdata_encoded = cacheParams.dataCode.encode(wdata)
      wdata_encoded
    })
  }


  // write dcache if hit
  val data_write = io.data_write.bits
  io.data_write.valid := s2_valid && s2_hit
  data_write.rmask    := DontCare
  data_write.way_en   := s2_tag_match_way
  data_write.addr     := s2_req.addr
  data_write.wmask    := VecInit((0 until blockRows) map (i => ~0.U(rowWords.W)))
  data_write.data     := wdata_merged

  assert(!(io.data_write.valid && !io.data_write.ready))

  val resp = Wire(Valid(new DCacheResp))
  resp.valid     := s2_valid
  resp.bits.data := DontCare
  resp.bits.meta := s2_req.meta
  resp.bits.miss := !s2_hit
  resp.bits.nack := s2_nack

  io.lsu.resp.valid := resp.valid
  io.lsu.resp.bits  := resp.bits
  assert(!(resp.valid && !io.lsu.resp.ready))

  when (resp.valid) {
    XSDebug(s"StorePipe resp: data: %x id: %d replay: %b miss: %b nack: %b\n",
      resp.bits.data, resp.bits.meta.id, resp.bits.meta.replay, resp.bits.miss, resp.bits.nack)
  }

  io.inflight_req_idxes(0).valid := io.lsu.req.valid
  io.inflight_req_idxes(1).valid := s1_valid
  io.inflight_req_idxes(2).valid := s2_valid

  io.inflight_req_idxes(0).bits  := get_idx(s0_req.addr)
  io.inflight_req_idxes(1).bits  := get_idx(s1_req.addr)
  io.inflight_req_idxes(2).bits  := get_idx(s2_req.addr)

  io.inflight_req_block_addrs(0).valid := io.lsu.req.valid
  io.inflight_req_block_addrs(1).valid := s1_valid
  io.inflight_req_block_addrs(2).valid := s2_valid

  io.inflight_req_block_addrs(0).bits  := get_block_addr(s0_req.addr)
  io.inflight_req_block_addrs(1).bits  := get_block_addr(s1_req.addr)
  io.inflight_req_block_addrs(2).bits  := get_block_addr(s2_req.addr)

  // -------
  // Debug logging functions
  def dump_pipeline_reqs(pipeline_stage_name: String, valid: Bool, req: DCacheLineReq ) = {
      when (valid) {
        XSDebug(
          s"$pipeline_stage_name cmd: %x addr: %x id: %d replay: %b\n",
          req.cmd, req.addr, req.meta.id, req.meta.replay
        )
      }
  }

  def dump_pipeline_valids(pipeline_stage_name: String, signal_name: String, valid: Bool) = {
    when (valid) {
      XSDebug(p"$pipeline_stage_name $signal_name " + s1_info)
    }
  }
}
