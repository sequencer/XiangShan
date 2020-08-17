package xiangshan.frontend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.ExcitingUtils._
import utils._
import xiangshan._

trait HasLoopBufferParameter extends HasXSParameter {
  val immLen = 12 // J-type and B-type max imm length = 12
  val offsetLen = log2Up(IBufSize) + 1
  val offsetBitPat = "1"*(immLen - offsetLen) + "?"*offsetLen
  val jalBitPat = "b1" + offsetBitPat.substring(2, 12) + offsetBitPat(1) + "1111111" + offsetBitPat(0) + "?????" + "1101111"
  val brBitPat = "b" + offsetBitPat(0) + offsetBitPat.substring(2, 8) + "?????????????" + offsetBitPat.substring(8, 12) + offsetBitPat(1) + "1100011"

  val LoopBufferSize = IBufSize * 2
  val LoopBufferIdxLen = log2Up(LoopBufferSize)
}

class IFUFetchIO extends XSBundle {
  val LBReq = Input(UInt(VAddrBits.W))
  val LBResp  = Output(new FakeIcacheResp)
}

class LoopBufferIO extends XSBundle {
  val flush = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchPacket))
  val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val LBredirect = ValidIO(UInt(VAddrBits.W))
  val inLoop = Output(Bool())
  val IFUFetch = new IFUFetchIO
}

class LoopBuffer extends XSModule with HasLoopBufferParameter{
  val io = IO(new LoopBufferIO)

  class IBufEntry extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val pnpc = UInt(VAddrBits.W)
    val brInfo = new BranchInfo
    val pd = new PreDecodeInfo
  }

  class LoopBufEntry extends XSBundle {
    val inst = UInt(16.W)
  }

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    io.out(i).bits.crossPageIPFFix := DontCare
  }

  // Check Short Backward Branch
  def isSBB(inst: UInt): Bool = {
    assert(IBufSize <= 1024)
    inst === BitPat(jalBitPat) || inst === BitPat(brBitPat)
  }

  // Get sbb target
  def SBBOffset(inst: UInt): UInt = {
    assert(IBufSize <= 1024)
    val isJal = inst === BitPat(jalBitPat)
    val isCon = inst === BitPat(brBitPat)
    Mux(isJal, Cat(Cat(inst(12), inst(20)), inst(27, 21))(LoopBufferIdxLen-1, 0), 
      Mux(isCon, Cat(Cat(Cat(inst(31), inst(7)), inst(30, 25)), inst(11, 8))(LoopBufferIdxLen-1, 0), 0.U(LoopBufferIdxLen.W)))
  }

  // predTaken to OH
  val predTakenVec = Mux(io.in.bits.predTaken, Reverse(PriorityEncoderOH(Reverse(io.in.bits.mask))), 0.U(PredictWidth.W))

  // Loop detect register
  val offsetCounter = Reg(UInt((LoopBufferIdxLen+1).W))
  val tsbbPC = RegInit(0.U(VAddrBits.W))

  val brTaken = ParallelOR((0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && predTakenVec(i))).asBool()
  val brIdx = OHToUInt(predTakenVec.asUInt)
  val sbbTaken = brTaken && isSBB(io.in.bits.instrs(brIdx))

  val tsbbVec = (0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && io.in.bits.pc(i) === tsbbPC)
  val tsbbIdx = OHToUInt(VecInit(tsbbVec).asUInt)
  val hasTsbb = ParallelOR(tsbbVec).asBool()
  val tsbbTaken = brTaken && io.in.bits.pc(brIdx) === tsbbPC

  // IBuffer define
  val ibuf = Mem(IBufSize, new IBufEntry)
  val ibuf_valid = RegInit(VecInit(Seq.fill(IBufSize)(false.B)))
  val head_ptr = RegInit(0.U(log2Up(IBufSize).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val enqValid = !io.flush && !ibuf_valid(tail_ptr + PopCount(io.in.bits.mask) - 1.U)
  val deqValid = !io.flush && ibuf_valid(head_ptr)

  // LoopBuffer define
  val loopBuf = Mem(LoopBufferSize, new LoopBufEntry)
  val loopBuf_valid = RegInit(VecInit(Seq.fill(LoopBufferSize)(false.B)))

  // FSM state define
  val s_idle :: s_fill :: s_active :: Nil = Enum(3)
  val LBstate = RegInit(s_idle)

  io.inLoop := LBstate === s_active

  def flushLB() = {
    for(i <- 0 until LoopBufferSize) {
      loopBuf(i).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      loopBuf_valid(i) := false.B
    }
  }

  def flushIB() = {
    for(i <- 0 until IBufSize) {
      ibuf(i).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      ibuf(i).pc := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      loopBuf(2*i).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      loopBuf(2*i+1).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      ibuf_valid(i) := false.B
    }
    head_ptr := 0.U
    tail_ptr := 0.U
  }

  def flush() = {
    XSDebug("Loop Buffer Flushed.\n")
    LBstate := s_idle
    flushLB
    flushIB
  }

  io.LBredirect.valid := false.B
  io.LBredirect.bits := DontCare

  /*---------------*/
  /*    Dequeue    */
  /*---------------*/
  var deq_idx = WireInit(head_ptr)

  when(deqValid) {
    for(i <- 0 until DecodeWidth) {
      var outWire = WireInit(ibuf(deq_idx))

      io.out(i).valid := ibuf_valid(deq_idx)
      when(ibuf_valid(deq_idx)) { ibuf_valid(deq_idx) := !io.out(i).fire }
      io.out(i).bits.instr := outWire.inst

      io.out(i).bits.pc := outWire.pc
      io.out(i).bits.brUpdate := DontCare
      io.out(i).bits.brUpdate.pc := outWire.pc
      io.out(i).bits.brUpdate.pnpc := outWire.pnpc
      io.out(i).bits.brUpdate.brInfo := outWire.brInfo
      io.out(i).bits.brUpdate.pd := outWire.pd

      deq_idx = deq_idx + io.out(i).fire
    }
    head_ptr := deq_idx
  }.otherwise {
    io.out.foreach(_.valid := false.B)
    io.out.foreach(_.bits <> DontCare)
  }

  /*---------------*/
  /*    Enqueue    */
  /*---------------*/
  io.in.ready := enqValid

  var enq_idx = WireInit(tail_ptr)
  when(io.in.fire) {
    // ExcitingUtils.addSource(LBstate =/= s_active, "CntFetchFromICache", Perf)
    // ExcitingUtils.addSource(LBstate === s_active, "CntFetchFromLoopBuffer", Perf)
    for(i <- 0 until PredictWidth) {
      var inWire = Wire(new IBufEntry)
      inWire := DontCare

      when(io.in.bits.mask(i)) {
        inWire.inst := io.in.bits.instrs(i)
        when(LBstate === s_fill/* || (sbbTaken && i.U > brIdx)*/) {
          loopBuf(io.in.bits.pc(i)(LoopBufferIdxLen,1)).inst := io.in.bits.instrs(i)(15, 0)
          // loopBuf(io.in.bits.pc(i)(LoopBufferIdxLen,1)).pd := io.in.bits.pd(i)
          loopBuf_valid(io.in.bits.pc(i)(LoopBufferIdxLen,1)) := true.B
          when(!io.in.bits.pd(i).isRVC) {
            loopBuf(io.in.bits.pc(i)(LoopBufferIdxLen,1) + 1.U).inst := io.in.bits.instrs(i)(31, 16)
            loopBuf_valid(io.in.bits.pc(i)(LoopBufferIdxLen,1) + 1.U) := true.B
          }
        }
        inWire.pc := io.in.bits.pc(i)
        inWire.pnpc := io.in.bits.pnpc(i)
        inWire.brInfo := io.in.bits.brInfo(i)
        inWire.pd := io.in.bits.pd(i)

        ibuf_valid(enq_idx) := Mux(LBstate =/= s_active, true.B, !(hasTsbb && !tsbbTaken && i.U > tsbbIdx))
        ibuf(enq_idx) := inWire
      }

      enq_idx = enq_idx + Mux(LBstate =/= s_active, io.in.bits.mask(i), io.in.bits.mask(i) && !(hasTsbb && !tsbbTaken && i.U > tsbbIdx))
    }

    tail_ptr := enq_idx
  }

  val pcStep = ParallelADD((0 until PredictWidth).map(i => Mux(!io.in.fire || !io.in.bits.mask(i), 0.U(log2Up(PredictWidth+1).W), Mux(io.in.bits.pd(i).isRVC, 1.U(log2Up(PredictWidth+1).W), 2.U(log2Up(PredictWidth+1).W)))))
  val offsetCounterWire = WireInit(offsetCounter + pcStep)
  offsetCounter := offsetCounterWire

  // IFU fetch from LB
  io.IFUFetch.LBResp.pc := io.IFUFetch.LBReq
  io.IFUFetch.LBResp.data := Cat((31 to 0 by -1).map(i => loopBuf(io.IFUFetch.LBReq(LoopBufferIdxLen,1) + i.U).inst))
  io.IFUFetch.LBResp.mask := Cat((31 to 0 by -1).map(i => loopBuf_valid(io.IFUFetch.LBReq(LoopBufferIdxLen,1) + i.U)))

  /*-----------------------*/
  /*    Loop Buffer FSM    */
  /*-----------------------*/
  when(io.in.fire) {
    switch(LBstate) {
      is(s_idle) {
        // To FILL
        // 检测到sbb且跳转，sbb成为triggering sbb
        when(sbbTaken) {
          LBstate := s_fill
          XSDebug("State change: FILL\n")
          offsetCounter := Cat("b1".U, SBBOffset(io.in.bits.instrs(brIdx)))
          tsbbPC := io.in.bits.pc(brIdx)
        }
      }
      is(s_fill) {
        // To AVTIVE
        // triggering sbb 造成cof
        when(offsetCounterWire(LoopBufferIdxLen) === 0.U){
          when(hasTsbb && tsbbTaken) {
            LBstate := s_active
            XSDebug("State change: ACTIVE\n")
          }.otherwise {
            LBstate := s_idle
            XSDebug("State change: IDLE\n")
            flushLB()
          }
        }

        when(brTaken && !tsbbTaken) {
          // To IDLE
          LBstate := s_idle
          XSDebug("State change: IDLE\n")
          flushLB()
        }
      }
      is(s_active) {
        // To IDLE
        // ExcitingUtils.addSource(hasTsbb && !tsbbTaken, "CntExitLoop", Perf)
        when(hasTsbb && brTaken && brIdx < tsbbIdx) {
          XSDebug("tsbb and cof in same fetchPacket\n")
          LBstate := s_idle
          io.LBredirect.valid := true.B
          io.LBredirect.bits := io.IFUFetch.LBReq
          XSDebug(p"redirect pc=${Hexadecimal(io.IFUFetch.LBReq)}\n")
          flushLB()

        }.elsewhen(hasTsbb && !tsbbTaken) {
          XSDebug("tsbb not taken, State change: IDLE\n")
          LBstate := s_idle
          io.LBredirect.valid := true.B
          io.LBredirect.bits := tsbbPC + 4.U
          XSDebug(p"redirect pc=${Hexadecimal(tsbbPC + 4.U)}\n")
          flushLB()

        }.elsewhen(brTaken && !tsbbTaken) {
          XSDebug("cof by other inst, State change: IDLE\n")
          LBstate := s_idle
          io.LBredirect.valid := true.B
          io.LBredirect.bits := io.IFUFetch.LBReq
          XSDebug(p"redirect pc=${Hexadecimal(io.IFUFetch.LBReq)}\n")
          flushLB()

        }
      }
    }
  }

  when(io.flush){
    flush()
  }

  // Debug Info
  XSDebug(io.flush, "LoopBuffer Flushed\n")

  XSDebug(LBstate === s_idle, "Current state: IDLE\n")
  XSDebug(LBstate === s_fill, "Current state: FILL\n")
  XSDebug(LBstate === s_active, "Current state: ACTIVE\n")

  XSDebug(p"offsetCounter = ${Binary(offsetCounterWire)}\n")
  when(io.in.fire) {
    XSDebug("Enque:\n")
    XSDebug(brTaken, p"Detected jump, idx=${brIdx}\n")
    XSDebug(p"predTaken=${io.in.bits.predTaken}, predTakenVec=${Binary(predTakenVec)}\n")
    XSDebug(p"MASK=${Binary(io.in.bits.mask)}\n")
    for(i <- 0 until PredictWidth){
        XSDebug(p"PC=${Hexadecimal(io.in.bits.pc(i))} ${Hexadecimal(io.in.bits.instrs(i))}\n")
    }
  }

  when(deqValid) {
    XSDebug("Deque:\n")
    for(i <- 0 until DecodeWidth){
        XSDebug(p"${Hexadecimal(io.out(i).bits.instr)}  PC=${Hexadecimal(io.out(i).bits.pc)}  v=${io.out(i).valid}  r=${io.out(i).ready}\n")
    }
  }

  XSDebug(p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")
  XSDebug("IBuffer:\n")
  for(i <- 0 until IBufSize/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
      ibuf(i*8+0).inst, ibuf_valid(i*8+0),
        ibuf(i*8+1).inst, ibuf_valid(i*8+1),
        ibuf(i*8+2).inst, ibuf_valid(i*8+2),
        ibuf(i*8+3).inst, ibuf_valid(i*8+3),
        ibuf(i*8+4).inst, ibuf_valid(i*8+4),
        ibuf(i*8+5).inst, ibuf_valid(i*8+5),
        ibuf(i*8+6).inst, ibuf_valid(i*8+6),
        ibuf(i*8+7).inst, ibuf_valid(i*8+7)
    )
  }

  XSDebug("LoopBuffer:\n")
  for(i <- 0 until LoopBufferSize/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
      loopBuf(i*8+0).inst, loopBuf_valid(i*8+0),
        loopBuf(i*8+1).inst, loopBuf_valid(i*8+1),
        loopBuf(i*8+2).inst, loopBuf_valid(i*8+2),
        loopBuf(i*8+3).inst, loopBuf_valid(i*8+3),
        loopBuf(i*8+4).inst, loopBuf_valid(i*8+4),
        loopBuf(i*8+5).inst, loopBuf_valid(i*8+5),
        loopBuf(i*8+6).inst, loopBuf_valid(i*8+6),
        loopBuf(i*8+7).inst, loopBuf_valid(i*8+7)
    )
  }
}