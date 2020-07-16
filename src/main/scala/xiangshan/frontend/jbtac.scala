package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.ALUOpType


class JBTACUpdateBundle extends XSBundle {
  val fetchPC = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(PredictWidth).W)
  val hist = UInt(HistoryLength.W)
  val target = UInt(VAddrBits.W)
  val _type = UInt(2.W)
  val misPred = Bool()
}

class JBTACPred extends XSBundle {
  val hit = Bool()
  val target = UInt(VAddrBits.W)
  val hitIdx = UInt(log2Up(PredictWidth).W)
}

class JBTAC extends XSModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val pc = Flipped(Decoupled(UInt(VAddrBits.W)))
      val pcLatch = Input(UInt(VAddrBits.W))
      val hist = Input(UInt(HistoryLength.W))
    }
    val redirectValid = Input(Bool())
    val flush = Input(Bool())
    val update = Input(new JBTACUpdateBundle)

    val out = Output(new JBTACPred)
  })

  io.in.pc.ready := true.B

  val fireLatch = RegNext(io.in.pc.fire())
  val nextFire = Wire(Bool())
  nextFire := fireLatch

  // JBTAC, divided into 8 banks, makes prediction for indirect jump except ret.
  val jbtacAddr = new TableAddr(log2Up(JbtacSize), JbtacBanks)
  def jbtacEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(jbtacAddr.tagBits.W + jbtacAddr.idxBits.W)
    val target = UInt(VAddrBits.W)
    val offset = UInt(log2Up(PredictWidth).W)
  }

  val jbtac = List.fill(JbtacBanks)(Module(new SRAMTemplate(jbtacEntry(), set = JbtacSize / JbtacBanks, shouldReset = true, holdRead = true, singlePort = false)))

  val readEntries = Wire(Vec(JbtacBanks, jbtacEntry()))

  val readFire = Reg(Vec(JbtacBanks, Bool()))
  // Only read one bank
  val histXORAddr = io.in.pc.bits ^ Cat(io.in.hist, 0.U(1.W))(VAddrBits - 1, 0)
  val histXORAddrLatch = RegEnable(histXORAddr, io.in.pc.valid)

  val readBank = jbtacAddr.getBank(histXORAddr)
  val readRow = jbtacAddr.getBankIdx(histXORAddr)
  readFire := 0.U.asTypeOf(Vec(JbtacBanks, Bool()))
  (0 until JbtacBanks).map(
    b => {
      jbtac(b).reset := reset.asBool
      jbtac(b).io.r.req.valid := io.in.pc.fire() && b.U === readBank
      jbtac(b).io.r.req.bits.setIdx := readRow
      readFire(b) := jbtac(b).io.r.req.fire()
      readEntries(b) := jbtac(b).io.r.resp.data(0)
    }
  )

  val readBankLatch = jbtacAddr.getBank(histXORAddrLatch)
  val readRowLatch = jbtacAddr.getBankIdx(histXORAddrLatch)

  val outHit = readEntries(readBankLatch).valid && 
    readEntries(readBankLatch).tag === Cat(jbtacAddr.getTag(io.in.pcLatch), jbtacAddr.getIdx(io.in.pcLatch)) &&
    !io.flush && readFire(readBankLatch)

  io.out.hit := outHit
  io.out.hitIdx := readEntries(readBankLatch).offset(log2Up(PredictWidth)-1, 1)
  io.out.target := readEntries(readBankLatch).target

    // 2. update jbtac
  val writeEntry = Wire(jbtacEntry())
  // val updateHistXORAddr = updatefetchPC ^ Cat(r.hist, 0.U(2.W))(VAddrBits - 1, 0)
  val updateHistXORAddr = io.update.fetchPC ^ Cat(io.update.hist, 0.U(1.W))(VAddrBits - 1, 0)
  writeEntry.valid := true.B
  // writeEntry.tag := jbtacAddr.getTag(updatefetchPC)
  writeEntry.tag := Cat(jbtacAddr.getTag(io.update.fetchPC), jbtacAddr.getIdx(io.update.fetchPC))
  writeEntry.target := io.update.target
  // writeEntry.offset := updateFetchIdx
  writeEntry.offset := io.update.fetchIdx

  val writeBank = jbtacAddr.getBank(updateHistXORAddr)
  val writeRow = jbtacAddr.getBankIdx(updateHistXORAddr)
  val writeValid = io.redirectValid && io.update.misPred && io.update._type === BTBtype.I
  for (b <- 0 until JbtacBanks) {
    when (b.U === writeBank) {
      jbtac(b).io.w.req.valid := writeValid
      jbtac(b).io.w.req.bits.setIdx := writeRow
      jbtac(b).io.w.req.bits.data := writeEntry
    }.otherwise {
      jbtac(b).io.w.req.valid := false.B
      jbtac(b).io.w.req.bits.setIdx := DontCare
      jbtac(b).io.w.req.bits.data := DontCare
    }
  }

  XSDebug(io.in.pc.fire(), "[JBTAC]read: pc=0x%x, histXORAddr=0x%x, bank=%d, row=%d, hist=%b\n",
    io.in.pc.bits, histXORAddr, readBank, readRow, io.in.hist)
  XSDebug(nextFire, "[JBTAC]read_resp: pc=0x%x, bank=%d, row=%d, target=0x%x, offset=%d, hit=%d\n",
    io.in.pcLatch, readBankLatch, readRowLatch, readEntries(readBankLatch).target, readEntries(readBankLatch).offset, outHit)
  XSDebug(io.redirectValid, "[JBTAC]update_req: fetchPC=0x%x, writeValid=%d, hist=%b, bank=%d, row=%d, target=0x%x, offset=%d, type=0x%d\n",
    io.update.fetchPC, writeValid, io.update.hist, writeBank, writeRow, io.update.target, io.update.fetchIdx, io.update._type)
}