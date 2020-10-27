package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import utils._

trait HasPrefetcherConst {
  val streamCnt = 4
  val streamSize = 4
  val ageWidth = 4
}

// abstract class PrefetcherModule extends DCacheModule with HasPrefetcherConst
// abstract class PrefetcherBundle extends DCacheBundle with HasPrefetcherConst
abstract class PrefetcherModule extends L1CacheModule
  with HasDCacheParameters
  with HasPrefetcherConst
abstract class PrefetcherBundle extends L1CacheBundle
  with HasDCacheParameters
  with HasPrefetcherConst
  
class PrefetcherIO extends PrefetcherBundle {
  // val req = Flipped(ValidIO(new MissReq))
  val in = Flipped(ValidIO(new Bundle {
    val req = new MissReq
    val miss = Bool()
  }))

  val prefetch_req = DecoupledIO(new MissReq)
  val prefetch_resp = Flipped(DecoupledIO(new MissResp))
  val prefetch_finish = DecoupledIO(new MissFinish)
}

class FakePrefetcher extends PrefetcherModule {
  val io = IO(new PrefetcherIO)

  io.prefetch_req.valid := false.B
  io.prefetch_req.bits := DontCare
  io.prefetch_resp.ready := true.B
  io.prefetch_finish.valid := false.B
  io.prefetch_finish.bits := DontCare

  assert(!io.prefetch_resp.fire(), "Fake prefetcher should not receive resp!")
}

class NextLinePrefetcher extends PrefetcherModule {
  val io = IO(new PrefetcherIO)

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val req = Reg(new MissReq)
  val resp = Reg(new MissResp)

  io.prefetch_req.valid := state === s_req
  // io.prefetch_req.bits.cmd := Mux(isRead(req.cmd), M_PFR, M_PFW)
  io.prefetch_req.bits.cmd := req.cmd
  io.prefetch_req.bits.addr := req.addr + (CacheLineSize / 8).U
  // io.prefetch_req.bits.client_id := Cat(3.U(clientIdWidth.W), 0.U(clientMissQueueEntryIdWidth.W))
  io.prefetch_req.bits.client_id := DontCare

  io.prefetch_resp.ready := state === s_resp

  io.prefetch_finish.valid := state === s_finish
  io.prefetch_finish.bits.client_id := resp.client_id
  io.prefetch_finish.bits.entry_id := resp.entry_id

  when (state === s_idle) {
    when (io.in.valid) {
      state := s_req
      req := io.in.bits.req
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

class StreamBufferEntry extends PrefetcherBundle {
  val req = new MissReq
  val resp = new MissResp
  
  override def toPrintable: Printable = {
    p"0x${Hexadecimal(req.addr)}"
  }
}

class StreamBufferUpdateBundle extends MissReq with HasPrefetcherConst {
  val hitIdx = UInt(log2Up(streamSize).W) // or one hot?

  override def toPrintable: Printable = {
    p"cmd=${Hexadecimal(cmd)} addr=0x${Hexadecimal(addr)} client_id=${client_id} hitIdx=${hitIdx}"
  }
}

class StreamBufferAllocBundle extends MissReq {
  // nothing here
  override def toPrintable: Printable = {
    p"cmd=${Hexadecimal(cmd)} addr=0x${Hexadecimal(addr)} client_id=${client_id}"
  }
}

class CompareBundle extends PrefetcherBundle {
  val bits = UInt(ageWidth.W)
  val idx = UInt()
}

object ParallelMin {
  def apply[T <: Data](xs: Seq[CompareBundle]): CompareBundle = {
    ParallelOperation(xs, (a: CompareBundle, b: CompareBundle) => Mux(a.bits < b.bits, a, b))
  }
}

class StreamBuffer extends PrefetcherModule {
  val io = IO(new Bundle {
    val entryId = Input(UInt(clientMissQueueEntryIdWidth.W))
    // val addr = ValidIO(UInt(PAddrBits.W))
    val addrs = Vec(streamSize, ValidIO(UInt(PAddrBits.W)))
    val update = Flipped(ValidIO(new StreamBufferUpdateBundle))
    val alloc = Flipped(ValidIO(new StreamBufferAllocBundle))

    val prefetchReq = DecoupledIO(new MissReq)
    val prefetchResp = Flipped(DecoupledIO(new MissResp))
    val prefetchFinish = DecoupledIO(new MissFinish) 
  })

  val baseReq = RegInit(0.U.asTypeOf(Valid(new MissReq)))

  val buf = RegInit(VecInit(Seq.fill(streamSize)(0.U.asTypeOf(new StreamBufferEntry))))
  val valid = RegInit(VecInit(Seq.fill(streamSize)(false.B)))
  val head = RegInit(0.U(log2Up(streamSize).W))
  val tail = RegInit(0.U(log2Up(streamSize).W))
  val full = head === tail && valid(head)
  val empty = head === tail && !valid(head)

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // dequeue
  when (io.update.valid) {
    val hitIdx = io.update.bits.hitIdx
    when (!empty && valid(hitIdx)) {

      // hitIdx between head and tail
      val headBeforeHitIdx = head <= hitIdx && (hitIdx < tail || tail <= head)
      val hitIdxBeforeHead = hitIdx < tail && tail <= head
      when (headBeforeHitIdx) {
        (0 until streamSize).foreach(i => valid(i) := Mux(i.U >= head && i.U <= hitIdx, false.B, valid(i)))
      }

      when (hitIdxBeforeHead) {
        (0 until streamSize).foreach(i => valid(i) := Mux(i.U >= head || i.U <= hitIdx, false.B, valid(i)))
      }

      when (headBeforeHitIdx || hitIdxBeforeHead) {
        head := hitIdx + 1.U
        baseReq.valid := true.B
        baseReq.bits := buf(hitIdx).req
      }
    }
  }


  // enqueue: send prefetch request as long as stream buffer is not full
  val tailReq = Mux(empty, baseReq.bits, buf(tail - 1.U).req)
  val prefetchReq = WireInit(tailReq)
  prefetchReq.addr := tailReq.addr + (CacheLineSize / 8).U
  prefetchReq.client_id := Cat(0.U(clientIdWidth.W), io.entryId)

  when (!full && state === s_idle && baseReq.valid) {
    state := s_req
    buf(tail).req := prefetchReq
  }

  when (state === s_req) {
    when (io.prefetchReq.fire()) {
      state := s_resp
    }
  }

  when (state === s_resp) {
    when (io.prefetchResp.fire()) {
      state := s_finish
      buf(tail).resp := io.prefetchResp.bits
    }
  }

  when (state === s_finish) {
    when (io.prefetchFinish.fire()) {
      state := s_idle
      valid(tail) := true.B
      tail := tail + 1.U
    }
  }


  // initialize: empty buffer when state === s_idle
  val needRealloc = RegInit(false.B)
  val reallocReq = RegInit(0.U.asTypeOf(new StreamBufferAllocBundle))
  when ((io.alloc.valid || needRealloc) && state === s_idle) {
    valid.foreach(_ := false.B)
    head := 0.U
    tail := 0.U
    baseReq.valid := true.B
    baseReq.bits := Mux(io.alloc.valid, io.alloc.bits, reallocReq)
    needRealloc := false.B
  }.elsewhen (io.alloc.valid && state =/= s_idle) {
    needRealloc := true.B
    reallocReq := io.alloc.bits
  }

  for (i <- 0 until streamSize) {
    io.addrs(i).valid := baseReq.valid && valid(i)
    io.addrs(i).bits := get_block_addr(buf(i).req.addr)
  }
  io.prefetchReq.valid := state === s_req
  io.prefetchReq.bits := prefetchReq
  io.prefetchReq.bits.addr := get_block_addr(prefetchReq.addr)
  io.prefetchResp.ready := state === s_resp
  io.prefetchFinish.valid := state === s_finish
  io.prefetchFinish.bits.client_id := buf(tail).resp.client_id
  io.prefetchFinish.bits.entry_id := buf(tail).resp.entry_id


  // debug
  XSDebug(VecInit(io.addrs.map(_.valid)).asUInt.orR, "addrs: ")
  for (i <- 0 until streamSize) {
    XSDebug(false, VecInit(io.addrs.map(_.valid)).asUInt.orR, "v:%d 0x%x  ", io.addrs(i).valid, io.addrs(i).bits)
  }
  XSDebug(false, VecInit(io.addrs.map(_.valid)).asUInt.orR, "\n")

  XSDebug(io.update.valid, p"update: ${io.update.bits}\n")
  XSDebug(io.alloc.valid, p"alloc: ${io.alloc.bits}\n")
  XSDebug("prefetchReq(%d %d) cmd=%x addr=0x%x client_id=%b\n",
    io.prefetchReq.valid, io.prefetchReq.ready, io.prefetchReq.bits.cmd, io.prefetchReq.bits.addr, io.prefetchReq.bits.client_id)
  XSDebug("prefetchResp(%d %d) client_id=%b entry_id=%b way_en=%b has_data=%d\n",
    io.prefetchResp.valid, io.prefetchResp.ready, io.prefetchResp.bits.client_id, io.prefetchResp.bits.entry_id, io.prefetchResp.bits.way_en, io.prefetchResp.bits.has_data)
  XSDebug("prefetchFinish(%d %d) client_id=%b entry_id=%b\n",
    io.prefetchFinish.valid, io.prefetchFinish.ready, io.prefetchFinish.bits.client_id, io.prefetchFinish.bits.entry_id)

  XSDebug("buf:\n")
  for (i <- 0 until streamSize) {
    if (i % 4 == 0) { XSDebug("") }
    XSDebug(false, true.B, p"${Hexadecimal(i.U)} v ${valid(i)} ${buf(i)}  ")
    if (i % 4 == 3) { XSDebug(false, true.B, "\n") }
  }
  XSDebug("state=%d head=%d tail=%d full=%d empty=%d\n", state, head, tail, full, empty)
  XSDebug(baseReq.valid, "baseReq: cmd=%x addr=0x%x client_id=%b\n", baseReq.bits.cmd, baseReq.bits.addr, baseReq.bits.client_id)
  XSDebug(needRealloc, p"reallocReq: ${reallocReq}\n")
  
}

class StreamPrefetcher extends PrefetcherModule {
  val io = IO(new PrefetcherIO)

  val streamBufs = Seq.fill(streamCnt) { Module(new StreamBuffer) }
  val addrValids = Wire(Vec(streamCnt, Vec(streamSize, Bool())))
  for (i <- 0 until streamCnt) {
    for (j <- 0 until streamSize) {
      addrValids(i)(j) := streamBufs(i).io.addrs(j).valid
    }
  }
  val bufValids = WireInit(VecInit(addrValids.map(_.asUInt.orR)))
  val ages = Seq.fill(streamCnt)(RegInit(0.U(ageWidth.W)))
  val maxAge = -1.S(ageWidth.W).asUInt

  // a buffer to record whether we find a stream of 2 adjacent cache lines
  def beforeEnterBufEntry = new Bundle {
    val conf = UInt(2.W)
    val addr = UInt(PAddrBits.W)

    override def toPrintable: Printable = {
      p"${conf} 0x${Hexadecimal(addr)}"
    }
  }
  val beforeEnterBuf = RegInit(VecInit(Seq.fill(streamCnt * 2)(0.U.asTypeOf(beforeEnterBufEntry))))

  // assign default values
  for (i <- 0 until streamCnt) {
    streamBufs(i).io.entryId := i.U
    streamBufs(i).io.update.valid := false.B
    streamBufs(i).io.update.bits := DontCare
    streamBufs(i).io.alloc.valid := false.B
    streamBufs(i).io.alloc.bits := DontCare
  }

  // 1. streamBufs hit
  val hit = WireInit(false.B)
  for (i <- 0 until streamCnt) {
    for (j <- 0 until streamSize) {
      when (io.in.valid && !io.in.bits.miss && addrValids(i)(j)) {
        when (streamBufs(i).io.addrs(j).bits === get_block_addr(io.in.bits.req.addr)) {
          hit := true.B
          streamBufs(i).io.update.valid := true.B
          streamBufs(i).io.update.bits.cmd := io.in.bits.req.cmd
          streamBufs(i).io.update.bits.addr := io.in.bits.req.addr
          streamBufs(i).io.update.bits.client_id := io.in.bits.req.client_id
          streamBufs(i).io.update.bits.hitIdx := j.U
          ages(i) := maxAge
        }
      }
    }
  }


  // 2. streamBufs miss
  // if this req has no adjacent cache line in beforeEnterBuf, set up a new entry and wait for next line;
  // otherwise, clear conf in beforeEnterBufEntry and set up a new stream buffer.

  // val reqLatch = RegNext(io.req)
  val inLatch = RegNext(io.in)
  val conf0Vec = VecInit(beforeEnterBuf.map(_.conf === 0.U))
  val conf1Vec = VecInit(beforeEnterBuf.map(_.conf === 1.U))
  val addrVec = VecInit(beforeEnterBuf.map(_.addr))
  val addrHits = VecInit(addrVec.map(_ === get_block_addr(inLatch.bits.req.addr))).asUInt
  val allocNewStream = WireInit(false.B)

  when (io.in.valid && io.in.bits.miss && !hit) {
    (0 until streamCnt).foreach(i => ages(i) := Mux(ages(i) =/= 0.U, ages(i) - 1.U, 0.U))
  }

  when (inLatch.valid && inLatch.bits.miss && !RegNext(hit)) {
    when ((conf1Vec.asUInt & addrHits).orR) {
      allocNewStream := true.B
      // clear off conf
      (0 until (streamCnt * 2)).foreach(i => beforeEnterBuf(i).conf := ~Fill(2, (conf1Vec(i) && addrHits(i)).asUInt) & beforeEnterBuf(i).conf)
    }.otherwise { // no adjacent line, set up a new entry in beforeEnterBuf
      val idx = Wire(UInt(log2Up(streamCnt*2).W))
      when (conf0Vec.asUInt.orR) {
        idx := PriorityMux(conf0Vec.asUInt, VecInit(List.tabulate(streamCnt*2)(_.U)))
      }.otherwise {
        idx := LFSR64()(log2Up(streamCnt*2) - 1, 0)
      }

      beforeEnterBuf(idx).conf := 1.U
      beforeEnterBuf(idx).addr := get_block_addr(inLatch.bits.req.addr) + (1.U << blockOffBits)
    }
  }

  when (allocNewStream) {
    val idx = Wire(UInt(log2Up(streamCnt).W))

    // refill an invalid or the eldest stream buffer with new one
    when ((~bufValids.asUInt).orR) {
      idx := PriorityMux(~bufValids.asUInt, VecInit(List.tabulate(streamCnt)(_.U)))
    }.otherwise {
      val ageSeq = Seq.fill(streamCnt)(Wire(new CompareBundle))
      for (i <- 0 until streamCnt) {
        ageSeq(i).bits := ages(i)
        ageSeq(i).idx := i.U
      }
      // idx := ParallelMin(ages.zipWithIndex.map{case (a,b) => (a, b.U)})._2
      idx := ParallelMin(ageSeq).idx
    }

    for (i <- 0 until streamCnt) {
      streamBufs(i).io.alloc.valid := idx === i.U
      streamBufs(i).io.alloc.bits := inLatch.bits.req
      streamBufs(i).io.alloc.bits.addr := get_block_addr(inLatch.bits.req.addr)
      when (idx === i.U) { ages(i) := maxAge }
    }
  }

  // 3. send prefetch req from stream buffer
  val reqArb = Module(new Arbiter(new MissReq, streamCnt))
  val finishArb = Module(new Arbiter(new MissFinish, streamCnt))
  for (i <- 0 until streamCnt) {
    reqArb.io.in(i).valid := streamBufs(i).io.prefetchReq.valid
    reqArb.io.in(i).bits := streamBufs(i).io.prefetchReq.bits
    streamBufs(i).io.prefetchReq.ready := reqArb.io.in(i).ready

    streamBufs(i).io.prefetchResp.valid := io.prefetch_resp.valid && io.prefetch_resp.bits.client_id(entryIdMSB, entryIdLSB) === i.U
    streamBufs(i).io.prefetchResp.bits := io.prefetch_resp.bits
    // io.prefetch_resp.ready ?????
    io.prefetch_resp.ready := Cat(streamBufs.map(_.io.prefetchResp.ready)).orR

    finishArb.io.in(i).valid := streamBufs(i).io.prefetchFinish.valid
    finishArb.io.in(i).bits := streamBufs(i).io.prefetchFinish.bits
    streamBufs(i).io.prefetchFinish.ready := finishArb.io.in(i).ready
  }

  io.prefetch_req <> reqArb.io.out

  io.prefetch_finish <> finishArb.io.out
  
  // debug
  XSDebug("clientIdWidth=%d clientMissQueueEntryIdWidth=%d\n", clientIdWidth.U, clientMissQueueEntryIdWidth.U)
  // XSDebug(io.req.valid, "missReq: cmd=%x addr=0x%x client_id=%b hit=%d\n", io.req.bits.cmd, io.req.bits.addr, io.req.bits.client_id, hit)
  XSDebug(io.in.valid, "in: cmd=%x addr=0x%x client_id=%b miss=%d  streanBufs hit=%d\n", io.in.bits.req.cmd, io.in.bits.req.addr, io.in.bits.req.client_id, io.in.bits.miss, hit)
  XSDebug("prefetch_req(%d %d) cmd=%x addr=0x%x client_id=%b\n",
    io.prefetch_req.valid, io.prefetch_req.ready, io.prefetch_req.bits.cmd, io.prefetch_req.bits.addr, io.prefetch_req.bits.client_id)
  XSDebug("prefetch_resp(%d %d) client_id=%b entry_id=%b way_en=%b has_data=%d\n",
    io.prefetch_resp.valid, io.prefetch_resp.ready, io.prefetch_resp.bits.client_id, io.prefetch_resp.bits.entry_id, io.prefetch_resp.bits.way_en, io.prefetch_resp.bits.has_data)
  XSDebug("prefetch_finish(%d %d) client_id=%b entry_id=%b\n",
    io.prefetch_finish.valid, io.prefetch_finish.ready, io.prefetch_finish.bits.client_id, io.prefetch_finish.bits.entry_id)
  
  XSDebug("")
  for (i <- 0 until streamCnt) {
    XSDebug(false, true.B, "%d: age=%d  ", i.U, ages(i))
  }
  XSDebug(false, true.B, "\n")

  XSDebug("beforeEnterBuf:\n")
  for (i <- 0 until (streamCnt*2)) {
    if (i % 4 == 0) { XSDebug("") }
    XSDebug(false, true.B, p"${beforeEnterBuf(i)} ")
    if (i % 4 == 3) { XSDebug(false, true.B, "\n") }
  }
  XSDebug("conf0Vec=%b conf1Vec=%b addrHits=%b\n", conf0Vec.asUInt, conf1Vec.asUInt, addrHits)

  XSDebug(inLatch.valid, "inLatch: cmd=%x addr=0x%x client_id=%b miss=%d  allocNewStream=%d\n", inLatch.bits.req.cmd, inLatch.bits.req.addr, inLatch.bits.req.client_id, inLatch.bits.miss, allocNewStream)

}

class ICachePrefetcher(enable: Boolean) extends PrefetcherModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Bundle {
      val req = new IcacheMissReq
      val miss = Bool()
    }))

    val prefetch_req = DecoupledIO(new IcacheMissReq)
    val prefetch_resp = Flipped(DecoupledIO(new IcacheMissResp))
  })

  val prefetcher = if (enable) Module(new StreamPrefetcher) else Module(new FakePrefetcher)

  io.in.ready := true.B
  prefetcher.io.in.valid := io.in.fire()
  prefetcher.io.in.bits := DontCare
  prefetcher.io.in.bits.req.cmd := M_XRD
  prefetcher.io.in.bits.req.addr := io.in.bits.req.addr
  prefetcher.io.in.bits.miss := io.in.bits.miss

  io.prefetch_req.valid := prefetcher.io.prefetch_req.valid
  io.prefetch_req.bits := DontCare
  io.prefetch_req.bits.addr := prefetcher.io.prefetch_req.bits.addr
  io.prefetch_req.bits.waymask := -1.S.asUInt
  io.prefetch_req.bits.client_id := prefetcher.io.prefetch_req.bits.client_id
  prefetcher.io.prefetch_req.ready := io.prefetch_req.ready

  prefetcher.io.prefetch_resp.valid := io.prefetch_resp.valid
  prefetcher.io.prefetch_resp.bits := DontCare
  prefetcher.io.prefetch_resp.bits.client_id := io.prefetch_resp.bits.client_id
  io.prefetch_resp.ready := prefetcher.io.prefetch_resp.ready

  prefetcher.io.prefetch_finish.ready := true.B
}
