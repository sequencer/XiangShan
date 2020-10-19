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

abstract class PrefetcherModule extends DCacheModule with HasPrefetcherConst
abstract class PrefetcherBundle extends DCacheBundle with HasPrefetcherConst

class PrefetcherIO extends PrefetcherBundle {
  val req = Flipped(ValidIO(new MissReq))

  val prefetch_req = DecoupledIO(new MissReq)
  val prefetch_resp = Flipped(DecoupledIO(new MissResp))
  val prefetch_finish = DecoupledIO(new MissFinish)
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

class StreamBufferEntry extends PrefetcherBundle {
  val req = new MissReq
  val resp = new MissResp
}

class StreamBufferUpdateBundle extends MissReq {
  // nothing here
}

class StreamBufferAllocBundle extends MissReq {
  // nothing here
}

object ParallelMin {
  def apply[T <: Data](xs: Seq[T]): T = {
    ParallelOperation(xs, (a: T, b: T) => Mux(a._1.asUInt < b._1.asUInt, a, b).asTypeOf(xs.head))
  }
}

class StreamBuffer extends PrefetcherModule {
  val io = IO(new Bundle {
    val entryId = Input(UInt(clientMissQueueEntryIdWidth.W))
    val addr = ValidIO(UInt(PAddrBits.W))
    val update = Flipped(ValidIO(new StreamBufferUpdateBundle))
    val alloc = Flipped(ValidIO(new StreamBufferAllocBundle))

    val prefetchReq = DecoupledIO(new MissReq)
    val prefetchResp = Flipped(DecoupledIO(new MissResp))
    val prefetchFinish = DecoupledIO(new MissFinish) 
  })

  val baseReq = RegInit(0.U.asTypeOf(Valid(new MissReq)))

  val buf = RegInit(VecInit(streamSize, 0.U.asTypeOf(new StreamBufferEntry)))
  val valid = RegInit(VecInit(Seq.fill(streamSize)(false.B)))
  val head = RegInit(0.U(log2Up(streamSize).W))
  val tail = RegInit(0.U(log2Up(streamSize).W))
  val full = head === tail && valid(head)
  val empty = head === tail && !valid(head)

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // dequeue
  when (io.update.valid) {
    when (!empty && valid(head)) {
      valid(head) := false.B
      head := head + 1.U
    }
    // does baseReq need to be updated?
  }


  // enqueue: send prefetch request as long as stream buffer is not full
  val tailReq = Mux(empty, baseReq.bits, buf(tail - 1.U).req)
  val prefetchReq = WireInit(tailReq)
  prefetchReq.addr := tailReq.addr + (CacheLineSize / 8).U
  prefetchReq.client_id := Cat(0.U(clientIdWidth.W), io.entryId)

  when (!full && state === s_idle) {
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
  val reallocReq = RegInit(0.U.asTypeOf(new MissReq))
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

  io.addr.valid := baseReq.valid && !empty && valid(head)
  io.addr.bits := get_block_addr(buf(head).req.addr)
  io.prefetchReq.valid := state === s_req
  io.prefetchReq.bits := prefetchReq
  io.prefetchResp.ready := state === s_resp
  io.prefetchFinish.valid := state === s_finish
  io.prefetchFinish.bits.client_id := buf(tail).resp.client_id
  io.prefetchFinish.bits.entry_id := buf(tail).resp.entry_id

}

class StreamBufferPrefetcher extends PrefetcherModule {
  val io = IO(new PrefetcherIO)

  val streamBufs = Seq.fill(streamCnt) { Module(new StreamBuffer) }
  val valids = VecInit(streamBufs.map(_.io.addr.valid))
  val ages = RegInit(VecInit(streamCnt, 0.U(ageWidth.W)))
  val maxAge = Fill(ageWidth, 1.U(1.W))

  // a buffer to record whether we find a stream of 2 adjacent cache lines
  def beforeEnterBufEntry = new Bundle {
    val conf = UInt(2.W)
    val addr = UInt(PAddrBits.W)

    override def toPrintable: Printable = {
      p"conf:${conf} addr:0x${Hexadecimal(addr)}"
    }
  }
  val beforeEnterBuf = RegInit(VecInit(Seq.fill(streamCnt * 2)(0.U.asTypeOf(new beforeEnterBufEntry))))

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
    when (io.req.valid && valids(i)) {
      when (streamBufs(i).io.addr.bits === get_block_addr(io.req.bits.addr)) {
        hit := true.B
        streamBufs(i).io.update.valid := true.B
        streamBufs(i).io.update.bits := io.req.bits
        ages(i) := maxAge
      }
    }
  }

  // 2. streamBufs miss
  // if this req has no adjacent cache line in beforeEnterBuf, set up a new entry and wait for next line;
  // otherwise, clear conf in beforeEnterBufEntry and set up a new stream buffer.
  val reqLatch = RegNext(io.req)
  val conf0Vec = VecInit(beforeEnterBuf.map(_.conf === 0.U))
  val conf1Vec = VecInit(beforeEnterBuf.map(_.conf === 1.U))
  val addrVec = VecInit(beforeEnterBuf.map(_.addr))
  val addrHits = VecInit(addrVec.map(_ === get_block_addr(reqLatch.bits.addr))).asUInt
  val allocNewStream = WireInit(false.B)

  when (io.req.valid && !hit) {
    (0 until streamCnt).foreach(i => ages(i) := ages(i) - 1.U)
  }

  when (reqLatch.valid && !RegNext(hit)) {
    when ((conf1Vec.asUInt & addrHits).orR) {
      allocNewStream := true.B
      // clear off conf
      (0 until (streamCnt * 2)).foreach(i => beforeEnterBuf(i).conf := ~Fill(2, (conf1Vec(i) && addrHits(i)).asUInt) & beforeEnterBuf(i).conf)
    }.otherwise { // no adjacent line, set up a new entry in beforeEnterBuf
      val idx = Wire(UInt(log2Up(streamCnt*2).W))
      when (conf0Vec.orR) {
        idx := PriorityMux(conf0Vec.asUInt, VecInit(List.tabulate(streamCnt*2)(_.U)))
      }.otherwise {
        idx := LFSR64()(log2Up(streamCnt*2) - 1, 0)
      }
      beforeEnterBuf(idx).conf := 1.U
      beforeEnterBuf(idx).addr := get_block_addr(reqLatch.bits.addr) + (1.U << blockOffBits)
    }
  }

  when (allocNewStream) {
    val idx = Wire(UInt(log2Up(streamCnt).W))

    // refill an invalid or the eldest stream buffer with new one
    when ((~valids.asUInt).orR) {
      idx := PriorityMux(~valids.asUInt, VecInit(List.tabulate(streamCnt)(_.U)))
    }.otherwise {
      idx := ParallelMin(ages.zipWidthIndex)._2.U
    }

    for (i <- 0 until streamCnt) {
      streamBufs(i).io.alloc.valid := idx === i.U
      streamBufs(i).io.alloc.bits := reqLatch.bits
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

    streamBufs(i).io.prefetchResp.valid := io.prefetch_resp.valid && io.prefetch_resp.bits.entry_id === i.U
    streamBufs(i).io.prefetchResp.bits := io.prefetch_resp.bits
    // io.prefetch_resp.ready ?????
    io.prefetch_resp.ready := streamBufs.map(_.io.prefetchResp.ready).orR

    finishArb.io.in(i).valid := streamBufs(i).io.prefetchFinish.valid
    finishArb.io.in(i).bits := streamBufs(i).io.prefetchFinish.bits
    streamBufs(i).io.prefetchFinish.ready := finishArb.io.in(i).ready
  }

  io.prefetch_req <> reqArb.io.out

  io.prefetch_finish <> finishArb.io.out
  
  // debug
}
