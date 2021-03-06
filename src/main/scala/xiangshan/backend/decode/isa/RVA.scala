package xiangshan.backend.decode.isa

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.LSUOpType
import xiangshan.backend.decode.HasInstrType


object RVAInstr extends HasInstrType {
  // Note: use instr(14,12) to distinguish D/W inst
  // def LR      = BitPat("b00010??00000_?????_???_?????_0101111")
  // def SC      = BitPat("b00011??00000_?????_???_?????_0101111")
  def LR_D      = BitPat("b00010_??_00000_?????_011_?????_0101111")
  def SC_D      = BitPat("b00011_??_?????_?????_011_?????_0101111")
  def AMOSWAP_D = BitPat("b00001_??_?????_?????_011_?????_0101111")
  def AMOADD_D  = BitPat("b00000_??_?????_?????_011_?????_0101111")
  def AMOXOR_D  = BitPat("b00100_??_?????_?????_011_?????_0101111")
  def AMOAND_D  = BitPat("b01100_??_?????_?????_011_?????_0101111")
  def AMOOR_D   = BitPat("b01000_??_?????_?????_011_?????_0101111")
  def AMOMIN_D  = BitPat("b10000_??_?????_?????_011_?????_0101111")
  def AMOMAX_D  = BitPat("b10100_??_?????_?????_011_?????_0101111")
  def AMOMINU_D = BitPat("b11000_??_?????_?????_011_?????_0101111")
  def AMOMAXU_D = BitPat("b11100_??_?????_?????_011_?????_0101111")

  def LR_W      = BitPat("b00010_??_00000_?????_010_?????_0101111")
  def SC_W      = BitPat("b00011_??_?????_?????_010_?????_0101111")
  def AMOSWAP_W = BitPat("b00001_??_?????_?????_010_?????_0101111")
  def AMOADD_W  = BitPat("b00000_??_?????_?????_010_?????_0101111")
  def AMOXOR_W  = BitPat("b00100_??_?????_?????_010_?????_0101111")
  def AMOAND_W  = BitPat("b01100_??_?????_?????_010_?????_0101111")
  def AMOOR_W   = BitPat("b01000_??_?????_?????_010_?????_0101111")
  def AMOMIN_W  = BitPat("b10000_??_?????_?????_010_?????_0101111")
  def AMOMAX_W  = BitPat("b10100_??_?????_?????_010_?????_0101111")
  def AMOMINU_W = BitPat("b11000_??_?????_?????_010_?????_0101111")
  def AMOMAXU_W = BitPat("b11100_??_?????_?????_010_?????_0101111")
  // funct3 === 010 or 011

  val table = Array(
    LR_D          -> List(InstrI, FuType.mou, LSUOpType.lr_d),
    SC_D          -> List(InstrSA, FuType.mou, LSUOpType.sc_d),
    AMOSWAP_D     -> List(InstrR, FuType.mou, LSUOpType.amoswap_d),
    AMOADD_D      -> List(InstrR, FuType.mou, LSUOpType.amoadd_d),
    AMOXOR_D      -> List(InstrR, FuType.mou, LSUOpType.amoxor_d),
    AMOAND_D      -> List(InstrR, FuType.mou, LSUOpType.amoand_d),
    AMOOR_D       -> List(InstrR, FuType.mou, LSUOpType.amoor_d),
    AMOMIN_D      -> List(InstrR, FuType.mou, LSUOpType.amomin_d),
    AMOMAX_D      -> List(InstrR, FuType.mou, LSUOpType.amomax_d),
    AMOMINU_D     -> List(InstrR, FuType.mou, LSUOpType.amominu_d),
    AMOMAXU_D     -> List(InstrR, FuType.mou, LSUOpType.amomaxu_d),

    LR_W          -> List(InstrI, FuType.mou, LSUOpType.lr_w),
    SC_W          -> List(InstrSA, FuType.mou, LSUOpType.sc_w),
    AMOSWAP_W     -> List(InstrR, FuType.mou, LSUOpType.amoswap_w),
    AMOADD_W      -> List(InstrR, FuType.mou, LSUOpType.amoadd_w),
    AMOXOR_W      -> List(InstrR, FuType.mou, LSUOpType.amoxor_w),
    AMOAND_W      -> List(InstrR, FuType.mou, LSUOpType.amoand_w),
    AMOOR_W       -> List(InstrR, FuType.mou, LSUOpType.amoor_w),
    AMOMIN_W      -> List(InstrR, FuType.mou, LSUOpType.amomin_w),
    AMOMAX_W      -> List(InstrR, FuType.mou, LSUOpType.amomax_w),
    AMOMINU_W     -> List(InstrR, FuType.mou, LSUOpType.amominu_w),
    AMOMAXU_W     -> List(InstrR, FuType.mou, LSUOpType.amomaxu_w),
  )
}
