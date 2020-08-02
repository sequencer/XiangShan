package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._

class LoopBufferIO extends XSBundle {
  val flush = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchPacket))
  val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val LBredirect = ValidIO(UInt(VAddrBits.W))
}

class LoopBuffer extends XSModule {
  val io = IO(new LoopBufferIO)

  class IBufEntry extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val fetchOffset = UInt((log2Up(FetchWidth * 4)).W)
    val pnpc = UInt(VAddrBits.W)
    val hist = UInt(HistoryLength.W)
    val btbPredCtr = UInt(2.W)
    val btbHit = Bool()
    val tageMeta = new TageMeta
    val rasSp = UInt(log2Up(RasSize).W)
    val rasTopCtr = UInt(8.W)
    val exceptionVec = Vec(16, Bool())
    val intrVec = Vec(12, Bool())
    val isRVC = Bool()
    val isBr = Bool()
    val crossPageIPFFix = Bool()

    // val valid = Bool()
    // val isTaken = Bool()
    val isLoop = Bool()
  }

  class LBufEntry extends XSBundle {
    val inst = UInt(16.W)
  }

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    io.out(i).bits.isBr := DontCare
    io.out(i).bits.crossPageIPFFix := DontCare
  }

  // Check Short Backward Branch
  def isSBB(inst: UInt): Bool = {
    inst === BitPat("b1111_???????_111111111_?????_1101111") || inst === BitPat("b1111???_?????_?????_???_????1_1100011")
  }

  // Get sbb target
  def SBBOffset(inst: UInt): UInt = {
    val isJal = inst === BitPat("b1111_???????_111111111_?????_1101111")
    val isCon = inst === BitPat("b1111???_?????_?????_???_????1_1100011")
    Mux(isJal, inst(27, 21), Mux(isCon, Cat(inst(27,25), inst(11,8)), 0.U(7.W)))
  }

  // FIXME: Can be replace by isBr
  def isBranch(inst: UInt): Bool = {
    inst === BitPat("b????????????????????_?????_1101111") || 
    inst === BitPat("b????????????????????_?????_1100111") || 
    inst === BitPat("b???????_?????_?????_???_?????_1100011")
  }

  val brTaken = ParallelOR((0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && io.in.bits.branchInfo(i))).asBool()
  val brIdx = OHToUInt(io.in.bits.branchInfo.asUInt)
  val sbbTaken = brTaken && isSBB(io.in.bits.instr(brIdx))

  val tsbbVec = (0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && io.in.bits.pc(i) === tsbbPC)
  val hasTsbb = ParallelOR(tsbbVec).asBool()
  val tsbbIdx = OHToUInt(tsbbVec.asUInt)
  val tsbbTaken = brTaken && brIdx === tsbbPC

  // IBuffer define
  val ibuf = Reg(Vec(IBufSize, new IBufEntry))
  val ibuf_valid = RegInit(VecInit(Seq.fill(IBufSize)(false.B)))
  val head_ptr = RegInit(0.U(log2Up(IBufSize).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val enqValid = !io.flush && !ibuf_valid(tail_ptr + FetchWidth.U - 1.U)
  val deqValid = !io.flush && ibuf_valid(head_ptr)

  // LoopBuffer define
  val lbuf = Reg(Vec(IBufSize*2, new LBufEntry))
  val lbuf_valid = RegInit(VecInit(Seq.fill(IBufSize*2)(false.B)))

  // Loop detect register
  val offsetCounter = Reg(UInt((log2Up(IBufSize)+2).W))
  val tsbbPC = Reg(UInt(VAddrBits.W))

  // FSM state define
  val s_idle :: s_fill :: s_active :: Nil = Enum(3)
  val LBstate = RegInit(s_idle)

  def flushLB() = {
    for(i <- 0 until IBufSize*2) {
      lbuf_valid(i) := false.B
    }
  }

  def flush() = {
    XSDebug("Loop Buffer Flushed.\n")
    LBstate := s_idle
    for(i <- 0 until IBufSize) {
      lbuf(i).inst := 0.U // TODO: This is to make the debugging information clearer and can be deleted
      lbuf(i).pc := 0.U // TODO: This is to make the debugging information clearer and can be deleted
      ibuf_valid(i) := false.B
      lbuf_valid(i) := false.B
    }
    head_ptr := 0.U
    tail_ptr := 0.U
  }

  io.LBredirect.valid := false.B
  io.LBredirect.bits := DontCare

  /*---------------*/
  /*    Dequeue    */
  /*---------------*/
  var deq_idx = WireInit(head_ptr)

  when(deqValid) {
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := ibuf_valid(deq_idx)
      ibuf_valid(deq_idx) := !io.out(i).fire
      when(ibuf(deq_idx).isLoop) {
        io.out(i).bits.instr := Mux(LBstate === s_active, 
          Cat(lbuf(ibuf(deq_idx).pc(7,1) + 1).inst, lbuf(ibuf(deq_idx).pc(7,1)).inst),
          ibuf(deq_idx).inst)
      }.otherwise {
        io.out(i).bits.instr := ibuf(deq_idx).inst
      }
      io.out(i).bits.pc := ibuf(deq_idx).pc
      io.out(i).bits.fetchOffset := ibuf(deq_idx).fetchOffset
      io.out(i).bits.pnpc := ibuf(deq_idx).pnpc
      io.out(i).bits.hist := ibuf(deq_idx).hist
      io.out(i).bits.btbPredCtr := ibuf(deq_idx).btbPredCtr
      io.out(i).bits.btbHit := ibuf(deq_idx).btbHit
      io.out(i).bits.tageMeta := ibuf(deq_idx).tageMeta
      io.out(i).bits.rasSp := ibuf(deq_idx).rasSp
      io.out(i).bits.rasTopCtr := ibuf(deq_idx).rasTopCtr
      io.out(i).bits.isRVC := false.B // FIXME: This is not ibuffer's work now

      deq_idx = deq_idx + io.out(i).fire
    }
    head_ptr := deq_idx
  }

  /*---------------*/
  /*    Enqueue    */
  /*---------------*/
  io.in.ready := enqValid

  var enq_idx = WireInit(tail_ptr)
  when(io.in.fire) {
    for(i <- 0 until PredictWidth) {
      val enqLB = LBstate === s_fill || (sbbTaken && i.U > brIdx)
      ibuf(enq_idx).inst := io.in.bits.instrs(i)
      ibuf(i).isLoop := enqLB
      when(enqLB) {
        lbuf(io.in.bits.pc(7,0)).inst := io.in.bits.instrs(i)(15, 0)
        lbuf_valid(io.in.bits.pc(7,0)) := true.B
        when(!io.in.bits.isRVC) {
          lbuf(io.in.bits.pc(7,0) + 1).inst := io.in.bits.instrs(i)(31, 16)
          lbuf_valid(io.in.bits.pc(7,0) + 1) := true.B
        }
      }
      ibuf(enq_idx).pc := io.in.bits.pc(i)
      ibuf(enq_idx).pnpc := io.in.bits.pnpc(i)
      ibuf(enq_idx).fetchOffset := ((enq_idx - tail_ptr)<<2).asUInt
      ibuf(enq_idx).hist := io.in.bits.hist(i)
      ibuf(enq_idx).btbPredCtr := io.in.bits.predCtr(i)
      ibuf(enq_idx).btbHit := io.in.bits.btbHit(i)
      ibuf(enq_idx).tageMeta := io.in.bits.tageMeta(i)
      ibuf(enq_idx).rasSp := io.in.bits.rasSp
      ibuf(enq_idx).rasTopCtr := io.in.bits.rasTopCtr

      ibuf_valid(enq_idx) := io.in.bits.mask(i)

      enq_idx = enq_idx + io.in.bits.mask(i)
    }

    tail_ptr := enq_idx
  }

  // val offsetCounterWire = WireInit(offsetCounter + (PopCount((0 until PredictWidth).map(io.in.bits.mask(i))) << 1).asUInt)
  val offsetCounterWire = WireInit(offsetCounter + ((0 until PredictWidth).map(i => PriorityMux(Seq(
    !(io.in.fire && io.in.bits.mask(i)) -> 0.U,
    io.in.bits.isRVC(i) -> 1.U,
    !io.in.bits.isRVC(i) -> 2.U,
  ))).reduce(_+&_)).asUInt)
  offsetCounter := offsetCounterWire

  /*-----------------------*/
  /*    Loop Buffer FSM    */
  /*-----------------------*/
  switch(LBstate) {
    is(s_idle) {
      // To FILL
      // 检测到sbb且跳转，sbb成为triggering sbb
      when(sbbTaken) {
        LBstate := s_fill
        XSDebug("State change: FILL\n")
        offsetCounter := Cat("b1".U, SBBOffset(io.out(sbbIdx).bits.instr)) + ((DecodeWidth.U - brIdx)<<1).asUInt
        tsbbPC := io.in.bits.pc(brIdx)
      }
    }
    is(s_fill) {
      // To AVTIVE
      // triggering sbb 造成cof
      when(offsetCounterWire((log2Up(IBufSize)+2)-1) === 0.U && hasTsbb){
        when(tsbbTaken) {
          LBstate := s_active
          XSDebug("State change: ACTIVE\n")
        }.otherwise {
          LBstate := s_idle
          XSDebug("State change: IDLE\n")
          flushLB
        }
      }

      when(brTaken && !tsbbTaken) {
        // To IDLE
        LBstate := s_idle
        XSDebug("State change: IDLE\n")
        flushLB
      }
    }
    is(s_active) {
      // To IDLE
      // triggering sbb不跳转
      when(hasTsbb && !tsbbTaken) {
        XSDebug("tsbb not taken, State change: IDLE\n")
        flush()
        io.LBredirect.valid := false.B
        io.LBredirect.bits := tsbbPC
      }

      when(brTaken && !tsbbTaken) {
        XSDebug("cof by other inst, State change: IDLE\n")
        flush()
      }
    }
  }

  when(io.flush){
    flush()
  }

  // Debug Info
  XSDebug(io.flush, "LoopBuffer Flushed\n")

  when(io.in.fire) {
    XSDebug("Enque:\n")
    XSDebug(p"PC=${Hexadecimal(io.in.bits.pc)} MASK=${Binary(io.in.bits.mask)}\n")
    for(i <- 0 until FetchWidth){
        XSDebug(p"${Hexadecimal(io.in.bits.instrs(i))}  v=${io.in.valid}  r=${io.in.ready}\n")
    }
  }

  when(deqValid) {
    XSDebug("Deque:\n")
    for(i <- 0 until DecodeWidth){
        XSDebug(p"${Hexadecimal(io.out(i).bits.instr)}  PC=${Hexadecimal(io.out(i).bits.pc)}  v=${io.out(i).valid}  r=${io.out(i).ready}\n")
    }
  }

  XSDebug(p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")
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
}