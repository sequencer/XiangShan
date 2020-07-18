package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._

class LoopBuffer extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val redirect = Input(Bool())
    val in = Flipped(DecoupledIO(new FetchPacket))
    val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
    val loopPC = Output(ValidIO(UInt(VAddrBits.W)))
  })

  class LBufEntry extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val valid = Bool()
    val isTaken = Bool()
  }

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits <> DontCare
  }

  // Check Short Backward Branch
  def isSBB(inst: UInt): Bool = {
    inst === BitPat("b1111_???????_11111111111111_1101111") || inst === BitPat("b1111???_?????_?????_???_????1_1100011")
  }

  // Get sbb target
  def SBBOffset(inst: UInt): UInt = {
    val isJal = inst === BitPat("b1111_???????_11111111111111_1101111")
    val isCon = inst === BitPat("b1111???_?????_?????_???_????1_1100011")
    Mux(isJal, inst(27, 21), Mux(isCon, Cat(inst(27,25), inst(11,8)), 0.U(7.W)))
  }

  def isJal(inst: UInt): Bool = {
    inst === BitPat("b????????????????????_?????_1101111") || inst === BitPat("b???????_?????_?????_???_?????_1100011")
//    false.B
  }

  //Count Register
  val offsetCounter = Reg(UInt((log2Up(IBufSize)+2).W))
  val tsbbPC = Reg(UInt(VAddrBits.W))

  def isFull(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2 && lbuf(ptr2).valid
  def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2 && !lbuf(ptr1).valid
  def isOverflow(ptr: UInt): Bool = lbuf(ptr).valid

  // Loop Buffer define
  val lbuf = Reg(Vec(IBufSize, new LBufEntry))
  val head_ptr = RegInit(0.U(log2Up(IBufSize).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val loop_str = RegInit(0.U(log2Up(IBufSize).W))
  val loop_end = RegInit(0.U(log2Up(IBufSize).W))
  val loop_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val hasSBB = (0 until DecodeWidth).map(i => lbuf(head_ptr + i.U).valid && isSBB(lbuf(head_ptr + i.U).inst)).reduce(_||_)
  val hasTSBB = (0 until DecodeWidth).map(i => io.out(i).valid && io.out(i).bits.pc === tsbbPC).reduce(_||_)
  val sbbIdx = OHToUInt(HighestBit(VecInit((0 until DecodeWidth).map(i => isSBB(lbuf(head_ptr + i.U).inst))).asUInt, DecodeWidth))
  val tsbbIdx = OHToUInt((0 until DecodeWidth).map(i => lbuf(head_ptr + i.U).pc === tsbbPC))
  val sbbTaken = lbuf(head_ptr + sbbIdx).isTaken
  val tsbbTaken = io.redirect
//  val tsbbTaken = lbuf(head_ptr + tsbbIdx).isTaken

  def flush() = {
    XSDebug("Loop Buffer Flushed.\n")
    LBstate := s_idle
    for(i <- 0 until IBufSize) {
      lbuf(i).inst := 0.U // Delete can improve performance?
      lbuf(i).pc := 0.U // Delete can improve performance?
      lbuf(i).valid := false.B
    }
    head_ptr := 0.U
    tail_ptr := 0.U
  }

  // clean invalid insts in LB when out FILL state
  def cleanFILL(str: UInt, end: UInt): Unit = {
    when(str <= end) {
      for(i <- 0 until IBufSize) {
        lbuf(i).valid := (str > i.U || i.U >= end) && lbuf(i).valid
      }
    }.otherwise {
      for(i <- 0 until IBufSize) {
        lbuf(i).valid := (str <= i.U && i.U < end) && lbuf(i).valid
      }
    }
  }

  val s_idle :: s_fill :: s_active :: Nil = Enum(3)
  val LBstate = RegInit(s_idle)

  // dequeue
  var deq_idx = 0.U(log2Up(DecodeWidth+2).W)

  when(LBstate =/= s_active) {
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := !isEmpty(head_ptr + deq_idx, tail_ptr)
      io.out(i).bits.instr := lbuf(head_ptr + deq_idx).inst
      io.out(i).bits.pc := lbuf(head_ptr + deq_idx).pc
      lbuf(head_ptr + deq_idx).valid := (lbuf(head_ptr + deq_idx).valid && LBstate === s_fill) || (hasSBB && sbbTaken && i.U > sbbIdx)

      deq_idx = deq_idx + (!isEmpty(head_ptr + deq_idx, tail_ptr) && io.out(i).fire)
    }

    head_ptr := head_ptr + deq_idx

    io.loopPC.valid := false.B
    io.loopPC.bits := DontCare
  }.otherwise {
    deq_idx = 0.U
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := deq_idx =/= DecodeWidth.U + 1.U && lbuf(loop_ptr + deq_idx).pc <= tsbbPC
      io.out(i).bits.instr := lbuf(loop_ptr + deq_idx).inst
      io.out(i).bits.pc := lbuf(loop_ptr + deq_idx).pc

      deq_idx = Mux(deq_idx === DecodeWidth.U + 1.U || loop_ptr + deq_idx === loop_end, DecodeWidth.U + 1.U, deq_idx + io.out(i).fire)
    }

    val next_loop_ptr = Mux(deq_idx === DecodeWidth.U + 1.U, loop_str, loop_ptr + deq_idx)
    loop_ptr := next_loop_ptr
//    XSDebug("deq_idx = %d\n", deq_idx)
//    XSDebug("loop_ptr = %d\n", Mux(deq_idx === DecodeWidth.U, loop_str, loop_ptr + deq_idx))
    io.loopPC.valid := true.B
    io.loopPC.bits := lbuf(next_loop_ptr).pc
  }

  val offsetCounterWire = WireInit(offsetCounter + (PopCount((0 until DecodeWidth).map(io.out(_).fire())) << 1).asUInt)
  offsetCounter := offsetCounterWire
  XSDebug("countReg=%b\n", offsetCounterWire)

  // enqueue
  var enq_idx = 0.U(log2Up(FetchWidth+1).W)

  io.in.ready := LBstate =/= s_active && !isOverflow(tail_ptr + FetchWidth.U - 1.U)

  when(io.in.fire()){
    for(i <- 0 until FetchWidth) {
      lbuf(tail_ptr + enq_idx).inst := io.in.bits.instrs(i)
      lbuf(tail_ptr + enq_idx).pc := io.in.bits.pc + (enq_idx << 2).asUInt
      lbuf(tail_ptr + enq_idx).valid := io.in.bits.mask(i<<1) // FIXME: need fix me when support RVC
      lbuf(tail_ptr + enq_idx).isTaken := io.redirect // isTaken can reduce to LBufSize/FetchWidth

      enq_idx = enq_idx + io.in.bits.mask(i<<1)
    }
    tail_ptr := tail_ptr + enq_idx
  }

  //Loop Buffer FSM
  switch(LBstate) {
    is(s_idle) {
      // To FILL
      // 检测到sbb且跳转，sbb成为triggrting sbb
      when(hasSBB && sbbTaken) {
        LBstate := s_fill
        XSDebug("State change: FILL\n")
        offsetCounter := Cat("b1".U, SBBOffset(lbuf(head_ptr + sbbIdx).inst)) + ((DecodeWidth.U - sbbIdx)<<1).asUInt
        tsbbPC := lbuf(head_ptr + sbbIdx).pc
        loop_str := head_ptr + sbbIdx + 1.U
        XSDebug("loop_str=%d\n", head_ptr + sbbIdx + 1.U)
      }
    }
    is(s_fill) {
      when(offsetCounterWire((log2Up(IBufSize)+2)-1) === 0.U && hasTSBB) {
        when(sbbTaken) {
          // To ACTIVE
          // triggering sbb造成cof
          LBstate := s_active
          XSDebug("State change: ACTIVE\n")
          loop_end := head_ptr + sbbIdx
          XSDebug("loop_end=%d\n", head_ptr + sbbIdx)
          loop_ptr := loop_str
        }.otherwise {
          // triggering sbb不跳转
          // To IDLE
          LBstate := s_idle
          cleanFILL(loop_str, head_ptr + PopCount((0 until DecodeWidth).map(io.out(_).fire())))
          XSDebug("State change: IDLE\n")
        }
      }

      // 非triggering sbb造成的cof
      // 遇到过一个周期内跳转为ACTIVE后又跳转为IDLE，无法重现
      when((0 until DecodeWidth).map(i => io.out(i).valid && !isSBB(io.out(i).bits.instr) && isJal(io.out(i).bits.instr) && lbuf(tail_ptr + i.U).isTaken).reduce(_||_)) {
        // To IDLE
        LBstate := s_idle
        cleanFILL(loop_str, head_ptr + PopCount((0 until DecodeWidth).map(io.out(_).fire())))
        XSDebug("State change: IDLE\n")
      }
    }
    is(s_active) {
      // To IDLE
      // triggering sbb不跳转
      when(hasTSBB && !tsbbTaken) {
        // To IDLE
        XSDebug("tsbb not taken, State change: IDLE\n")
        flush()
      }

      // 非triggering sbb造成的cof
      when((0 until DecodeWidth).map(i => io.out(i).valid && io.out(i).bits.pc =/= tsbbPC && isJal(io.out(i).bits.instr) && io.redirect).reduce(_||_)) {
        // To IDLE
        XSDebug("cof by other inst, State change: IDLE\n")
        flush()
      }
    }
  }

  // flush
  when(io.flush) {
    flush()
  }

  // Debug Info
  // XSDebug(io.in.fire(), p"PC= ${Hexadecimal(io.in.bits.pc)}\n")
  XSDebug(io.flush, "Loop Buffer Flushed\n")

  when(io.in.valid) {
    XSDebug(p"PC=${Hexadecimal(io.in.bits.pc)}\n")
    for(i <- 0 until FetchWidth){
      XSDebug(p"${Hexadecimal(io.in.bits.instrs(i))}  v=${io.in.valid}  r=${io.in.ready}\n")
    }
  }

  when((0 until DecodeWidth).map(i => io.out(i).ready).reduce(_||_)){
    for(i <- 0 until DecodeWidth){
      XSDebug(p"${Hexadecimal(io.out(i).bits.instr)}  PC=${Hexadecimal(io.out(i).bits.pc)}  v=${io.out(i).valid}  r=${io.out(i).ready}\n")
    }
  }

  XSDebug(p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")

  // Print loop buffer
  for(i <- 0 until IBufSize/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
      lbuf(i*8+0).inst, lbuf(i*8+0).valid,
      lbuf(i*8+1).inst, lbuf(i*8+1).valid,
      lbuf(i*8+2).inst, lbuf(i*8+2).valid,
      lbuf(i*8+3).inst, lbuf(i*8+3).valid,
      lbuf(i*8+4).inst, lbuf(i*8+4).valid,
      lbuf(i*8+5).inst, lbuf(i*8+5).valid,
      lbuf(i*8+6).inst, lbuf(i*8+6).valid,
      lbuf(i*8+7).inst, lbuf(i*8+7).valid
    )
  }
}
