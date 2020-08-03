package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._


class BrqPtr extends XSBundle {

  val flag = Bool()
  val value = UInt(BrTagWidth.W)

  final def + (inc: Bool): BrqPtr = {
    Mux(inc && (value === (BrqSize-1).U),
      BrqPtr(!flag, 0.U),
      BrqPtr(flag, value + inc)
    )
  }

  final def === (that: BrqPtr): Bool = {
    (this.value===that.value) && (this.flag===that.flag)
  }

  // this.age < that.age
  final def < (that: BrqPtr): Bool = {
    Mux(this.flag === that.flag,
      this.value > that.value,
      this.value < that.value
    )
  }

  def needBrFlush(redirectTag: BrqPtr): Bool = this < redirectTag

  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.valid && (redirect.bits.isException || needBrFlush(redirect.bits.brTag))
  }

  override def toPrintable: Printable = p"f:$flag v:$value"

}

object BrqPtr {
  def apply(f: Bool, v: UInt): BrqPtr = {
    val ptr = Wire(new BrqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class BrqIO extends XSBundle{
  // interrupt/exception happen, flush Brq
  val roqRedirect = Input(Valid(new Redirect))
  // receive branch/jump calculated target
  val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, Flipped(ValidIO(new ExuOutput)))
  // from decode, branch insts enq
  val enqReqs = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
  // to decode
  val brTags = Output(Vec(DecodeWidth, new BrqPtr))
  // to roq
  val out = ValidIO(new ExuOutput)
  // misprediction, flush pipeline
  val redirect = Output(Valid(new Redirect))
  // commit cnt of branch instr
  val bcommit = Input(UInt(BrTagWidth.W))
  // in order dequeue to train bpd
  val inOrderBrInfo = Output(new RedirectInfo)
}

class Brq extends XSModule {
  val io = IO(new BrqIO)

  class BrqEntry extends Bundle {
    val ptrFlag = Bool()
    val npc = UInt(VAddrBits.W)
    val misPred = Bool()
    val exuOut = new ExuOutput
  }

  val s_idle :: s_wb :: s_commited :: Nil  =
    List.tabulate(3)(i => (1 << i).U(3.W).asTypeOf(new StateQueueEntry))

  class StateQueueEntry extends Bundle{
    val isCommit = Bool()
    val isWb = Bool()
    val isIdle = Bool()
  }

  val brCommitCnt = RegInit(0.U(BrTagWidth.W))
  val brQueue = Reg(Vec(BrqSize, new BrqEntry))
  val stateQueue = RegInit(VecInit(Seq.fill(BrqSize)(s_idle)))

  val headPtr, tailPtr = RegInit(BrqPtr(false.B, 0.U))

  def isEmpty(ptr1: BrqPtr, ptr2: BrqPtr): Bool = ptr1 === ptr2
  def isFull(ptr1: BrqPtr, ptr2: BrqPtr): Bool = (ptr1.flag=/=ptr2.flag) && (ptr1.value===ptr2.value)


  // dequeue
  val headIdx = headPtr.value

  val skipMask = Cat(stateQueue.zipWithIndex.map({
    case (s, i) => (s.isWb && !brQueue(i).misPred) || s.isCommit
  }).reverse)

  /*
      example: headIdx       = 2
               headIdxOH     = 00000100
               headIdxMaskHI = 11111100
               headIdxMaskLo = 00000011
               skipMask      = 00111101
               commitIdxHi   =  6
               commitIdxLo   =        0
               commitIdx     =  6
   */
  val headIdxOH = UIntToOH(headIdx)
  val headIdxMaskHiVec = Wire(Vec(BrqSize, Bool()))
  for(i <- headIdxMaskHiVec.indices){
    headIdxMaskHiVec(i) := { if(i==0) headIdxOH(i) else headIdxMaskHiVec(i-1) || headIdxOH(i) }
  }
  val headIdxMaskHi = headIdxMaskHiVec.asUInt()
  val headIdxMaskLo = (~headIdxMaskHi).asUInt()

  val commitIdxHi = PriorityEncoder((~skipMask).asUInt() & headIdxMaskHi)
  val (commitIdxLo, findLo) = PriorityEncoderWithFlag((~skipMask).asUInt() & headIdxMaskLo)

  val skipHi = (skipMask | headIdxMaskLo) === Fill(BrqSize, 1.U(1.W))
  val useLo = skipHi && findLo

  val commitIdx = Mux(stateQueue(commitIdxHi).isWb && brQueue(commitIdxHi).misPred,
    commitIdxHi,
    Mux(useLo && stateQueue(commitIdxLo).isWb && brQueue(commitIdxLo).misPred,
      commitIdxLo,
      headIdx
    )
  )

  val commitIsHead = commitIdx===headIdx
  val deqValid = !stateQueue(headIdx).isIdle && commitIsHead && brCommitCnt=/=0.U
  val commitValid = stateQueue(commitIdx).isWb
  val commitEntry = brQueue(commitIdx)

  brCommitCnt := brCommitCnt + io.bcommit - deqValid

  XSDebug(p"brCommitCnt:$brCommitCnt\n")
  assert(brCommitCnt+io.bcommit >= deqValid)
  io.inOrderBrInfo.valid := deqValid
  io.inOrderBrInfo.misPred := commitEntry.misPred
  io.inOrderBrInfo.redirect := commitEntry.exuOut.redirect

//  XSDebug(
//    p"commitIdxHi:$commitIdxHi ${Binary(headIdxMaskHi)} ${Binary(skipMask)}\n"
//  )
//  XSDebug(
//    p"commitIdxLo:$commitIdxLo ${Binary(headIdxMaskLo)} ${Binary(skipMask)}\n"
//  )
  XSDebug(p"headIdx:$headIdx commitIdx:$commitIdx\n")
  XSDebug(p"headPtr:$headPtr tailPtr:$tailPtr\n")
  XSDebug("")
  stateQueue.reverse.map(s =>{
    XSDebug(false, s.isIdle, "-")
    XSDebug(false, s.isWb, "w")
    XSDebug(false, s.isCommit, "c")
  })
  XSDebug(false, true.B, "\n")

  val headPtrNext = WireInit(headPtr + deqValid)
  stateQueue(commitIdx):= Mux(deqValid,
    s_idle,
    Mux(commitValid,
      s_commited,
      stateQueue(commitIdx)
    )
  )

  headPtr := headPtrNext
  io.redirect.valid := commitValid && commitEntry.misPred && !io.roqRedirect.valid
  io.redirect.bits := commitEntry.exuOut.redirect
  io.out.valid := commitValid
  io.out.bits := commitEntry.exuOut
  XSInfo(io.out.valid,
    p"commit branch to roq, mispred:${io.redirect.valid} pc=${Hexadecimal(io.out.bits.uop.cf.pc)}\n"
  )

  // branch insts enq
  var full = WireInit(isFull(headPtrNext, tailPtr))
  var tailPtrNext = WireInit(tailPtr)
  for((enq, brTag) <- io.enqReqs.zip(io.brTags)){
    enq.ready := !full
    brTag := tailPtrNext
    when(enq.fire()){
      brQueue(tailPtrNext.value).npc := enq.bits.cf.pnpc
      brQueue(tailPtrNext.value).ptrFlag := tailPtrNext.flag
    }

    tailPtrNext = tailPtrNext + enq.fire()
    full = isFull(tailPtrNext, headPtrNext)
  }
  tailPtr := tailPtrNext

  // exu write back
  for(exuWb <- io.exuRedirect){
    when(exuWb.valid){
      val wbIdx = exuWb.bits.redirect.brTag.value
      XSInfo(
        p"exu write back: brTag:${exuWb.bits.redirect.brTag}" +
          p" pc=${Hexadecimal(exuWb.bits.uop.cf.pc)} pnpc=${Hexadecimal(brQueue(wbIdx).npc)} target=${Hexadecimal(exuWb.bits.redirect.target)}\n"
      )
      stateQueue(wbIdx) := s_wb
      brQueue(wbIdx).exuOut := exuWb.bits
      brQueue(wbIdx).misPred := brQueue(wbIdx).npc =/= exuWb.bits.redirect.target
      // brQueue(wbIdx).exuOut.redirect.hist := exuWb.bits.uop.cf.hist
      // brQueue(wbIdx).exuOut.redirect.btbVictimWay := exuWb.bits.uop.cf.btbVictimWay
      // brQueue(wbIdx).exuOut.redirect.btbPredCtr := exuWb.bits.uop.cf.btbPredCtr
      // brQueue(wbIdx).exuOut.redirect.btbHitWay := exuWb.bits.uop.cf.btbHitWay
      // brQueue(wbIdx).exuOut.redirect.tageMeta := exuWb.bits.uop.cf.tageMeta
      // brQueue(wbIdx).exuOut.redirect.rasSp := exuWb.bits.uop.cf.rasSp
      // brQueue(wbIdx).exuOut.redirect.rasTopCtr := exuWb.bits.uop.cf.rasTopCtr
      // brQueue(wbIdx).exuOut.redirect.fetchIdx := exuWb.bits.uop.cf.fetchOffset << 2.U
      brQueue(wbIdx).exuOut.redirect := exuWb.bits.redirect
    }
  }

  when(io.roqRedirect.valid){
    // exception
    stateQueue.foreach(_ := s_idle)
    headPtr := BrqPtr(false.B, 0.U)
    tailPtr := BrqPtr(false.B, 0.U)
    brCommitCnt := 0.U
  }.elsewhen(io.redirect.valid){
    // misprediction
    stateQueue.zipWithIndex.foreach({case(s, i) =>
      val ptr = BrqPtr(brQueue(i).ptrFlag, i.U)
      when(ptr.needBrFlush(io.redirect.bits.brTag)){
        s := s_idle
      }
    })
    tailPtr := io.redirect.bits.brTag + true.B
  }




  // Debug info
  val debug_roq_redirect = io.roqRedirect.valid
  val debug_brq_redirect = io.redirect.valid && !debug_roq_redirect
  val debug_normal_mode = !(debug_roq_redirect || debug_brq_redirect)

  for(i <- 0 until DecodeWidth){
    XSDebug(
      debug_normal_mode,
      p"enq v:${io.enqReqs(i).valid} rdy:${io.enqReqs(i).ready} pc:${Hexadecimal(io.enqReqs(i).bits.cf.pc)}" +
        p"pnpc:${Hexadecimal(io.enqReqs(i).bits.cf.pnpc)} brTag:${io.brTags(i)}\n"
    )
  }

  XSInfo(debug_roq_redirect, "roq redirect, flush brq\n")

  XSInfo(debug_brq_redirect, p"brq redirect, target:${Hexadecimal(io.redirect.bits.target)}\n")

  val fire = io.out.fire()
  val predRight = fire && !commitEntry.misPred
  val predWrong = fire && commitEntry.misPred
  val isBType = commitEntry.exuOut.redirect.btbType===BTBtype.B
  val isJType = commitEntry.exuOut.redirect.btbType===BTBtype.J
  val isIType = commitEntry.exuOut.redirect.btbType===BTBtype.I
  val isRType = commitEntry.exuOut.redirect.btbType===BTBtype.R
  val mbpInstr = fire
  val mbpRight = predRight
  val mbpWrong = predWrong
  val mbpBRight = predRight && isBType
  val mbpBWrong = predWrong && isBType
  val mbpJRight = predRight && isJType
  val mbpJWrong = predWrong && isJType
  val mbpIRight = predRight && isIType
  val mbpIWrong = predWrong && isIType
  val mbpRRight = predRight && isRType
  val mbpRWrong = predWrong && isRType

  if(!env.FPGAPlatform){
    ExcitingUtils.addSource(mbpInstr, "perfCntCondMbpInstr", Perf)
    ExcitingUtils.addSource(mbpRight, "perfCntCondMbpRight", Perf)
    ExcitingUtils.addSource(mbpWrong, "perfCntCondMbpWrong", Perf)
    ExcitingUtils.addSource(mbpBRight, "perfCntCondMbpBRight", Perf)
    ExcitingUtils.addSource(mbpBWrong, "perfCntCondMbpBWrong", Perf)
    ExcitingUtils.addSource(mbpJRight, "perfCntCondMbpJRight", Perf)
    ExcitingUtils.addSource(mbpJWrong, "perfCntCondMbpJWrong", Perf)
    ExcitingUtils.addSource(mbpIRight, "perfCntCondMbpIRight", Perf)
    ExcitingUtils.addSource(mbpIWrong, "perfCntCondMbpIWrong", Perf)
    ExcitingUtils.addSource(mbpRRight, "perfCntCondMbpRRight", Perf)
    ExcitingUtils.addSource(mbpRWrong, "perfCntCondMbpRWrong", Perf)
  }
}
