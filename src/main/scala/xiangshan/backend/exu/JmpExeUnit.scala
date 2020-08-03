package xiangshan.backend.exu

import chisel3._
import xiangshan.{ExuOutput, FuType}
import xiangshan.backend.fu.{CSR, Jump}
import xiangshan.backend.decode.isa._
import utils._

class JmpExeUnit extends Exu(Exu.jmpExeUnitCfg) {

  val jmp = Module(new Jump)

  jmp.io.out.ready := io.out.ready
  jmp.io.exception <> DontCare
  jmp.io.dmem <> DontCare
  jmp.io.scommit := DontCare
  jmp.io.redirect := io.redirect

  val csr = Module(new CSR)
  csr.io.cfIn := io.in.bits.uop.cf
  csr.io.fpu_csr := DontCare
  csr.io.exception <> io.exception
  csr.io.instrValid := DontCare
  csr.io.imemMMU := DontCare
  csr.io.dmemMMU := DontCare
  csr.io.out.ready := io.out.ready
  csr.io.in.bits.src3 := DontCare
  val csrOut = csr.access(
    valid = io.in.valid && io.in.bits.uop.ctrl.fuType===FuType.csr,
    src1 = io.in.bits.src1,
    src2 = io.in.bits.src2,
    func = io.in.bits.uop.ctrl.fuOpType
  )

  val csrExuOut = Wire(new ExuOutput)
  csrExuOut.uop := io.in.bits.uop
  csrExuOut.uop.cf := csr.io.cfOut
  csrExuOut.data := csrOut
  csrExuOut.redirectValid := csr.io.redirectValid
  csrExuOut.redirect.brTag := io.in.bits.uop.brTag
  csrExuOut.redirect.isException := false.B
  csrExuOut.redirect.roqIdx := io.in.bits.uop.roqIdx
  csrExuOut.redirect.target := csr.io.redirect.target
  csrExuOut.debug := DontCare

  val uop = io.in.bits.uop
  csrExuOut.redirect.pc := uop.cf.pc
  csrExuOut.redirect.brTarget := DontCare // DontCare
  csrExuOut.redirect.btbType := LookupTree(uop.ctrl.fuOpType, RV32I_BRUInstr.bruFuncTobtbTypeTable)
  csrExuOut.redirect.isRVC := uop.cf.isRVC
  csrExuOut.redirect.taken := false.B
  csrExuOut.redirect.hist := uop.cf.hist
  csrExuOut.redirect.tageMeta := uop.cf.tageMeta
  csrExuOut.redirect.fetchIdx := uop.cf.fetchOffset >> 1.U  //TODO: consider RVC
  csrExuOut.redirect.btbPredCtr := uop.cf.btbPredCtr
  csrExuOut.redirect.btbHit := uop.cf.btbHit
  csrExuOut.redirect.rasSp := uop.cf.rasSp
  csrExuOut.redirect.rasTopCtr := uop.cf.rasTopCtr

  jmp.io.in.bits := io.in.bits
  jmp.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.fuType===FuType.jmp

  io.in.ready := io.out.ready
  io.out.bits := Mux(jmp.io.in.valid, jmp.io.out.bits, csrExuOut)
  io.out.valid := io.in.valid
}