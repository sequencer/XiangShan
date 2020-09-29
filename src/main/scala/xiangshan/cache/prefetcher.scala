package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import utils._

abstract class Prefetcher extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new MissReq))

    val prefetch_req = DecoupledIO(new MissReq)
    val prefetch_resp = Flipped(DecoupledIO(new MissResp))
    val prefetch_finish = DecoupledIO(new MissFinish)
  })
}

class NextLinePrefetcher extends Prefetcher {

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val req = Reg(new MissReq)
  val resp = Reg(new MissResp)

  io.prefetch_req.valid := state === s_req
  io.prefetch_req.bits.cmd := Mux(isRead(req.cmd), M_PFR, M_PFW)
  io.prefetch_req.bits.addr := req.addr + (CacheLineSize / 8).U
  // io.prefetch_req.bits.client_id := Cat(3.U(clientIdWidth.W), 0.U(clientMissQueueEntryIdWidth.W))
  io.prefetch_req.bits.client_id := DontCare

  io.prefetch_resp.ready := state === s_resp

  io.prefetch_finish.valid := state === s_finish
  io.prefetch_finish.bits.client_id := resp.client_id
  io.prefetch_finish.bits.entry_id := resp.entry_id

  when (state === s_idle) {
    when (io.req.valid) {
      state := s_req
      req := io.req.bits
    }
  }

  when (state === s_req) {
    when (io.prefetch_req.fire()) {
      state := s_resp
    }
  }

  when (state === s_resp) {
    when (io.prefetch_resp.fire()) {
      state := s_finish
      resp := io.prefetch_resp.bits
    }
  }

  when (state === s_finish) {
    when (io.prefetch_finish.fire()) {
      state := s_idle
    }
  }
}