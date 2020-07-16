package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap

class BTBUpdateBundle extends XSBundle {
  // val fetchPC = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)
  // val fetchIdx = UInt(log2Up(FetchWidth*2).W)
  val hit = Bool()
  val misPred = Bool()
  // val writeWay = UInt(log2Up(BtbWays).W)
  val oldCtr = UInt(2.W)
  val taken = Bool()
  val target = UInt(VAddrBits.W)
  val _type = UInt(2.W)
  val isRVC = Bool()
}

class BTBPred extends XSBundle {
  val hit = Bool()
  val taken = Bool()
  val takenIdx = UInt(log2Up(FetchWidth).W)
  val target = UInt(VAddrBits.W)

  // val writeWay = UInt(log2Up(BtbWays).W)
  val notTakens = Vec(FetchWidth, Bool())
  val dEntries = Vec(FetchWidth, btbDataEntry())
  val hits = Vec(FetchWidth, Bool())
}

case class btbDataEntry() extends XSBundle {
  val target = UInt(VAddrBits.W)
  val pred = UInt(2.W) // 2-bit saturated counter as a quick predictor
  val _type = UInt(2.W)
  val isRVC = Bool()
}

case class btbMetaEntry() extends XSBundle {
  val valid = Bool()
  // TODO: don't need full length of tag
  val tag = UInt((VAddrBits - log2Up(BtbSize) - 1).W)
}

class BTB extends XSModule {
  val io = IO(new Bundle() {
    // Input
    val in = new Bundle {
      val pc = Flipped(Decoupled(UInt(VAddrBits.W)))
      val pcLatch = Input(UInt(VAddrBits.W))
      val mask = Input(UInt((PredictWidth).W))
    }
    val redirectValid = Input(Bool())
    val flush = Input(Bool())
    val update = Input(new BTBUpdateBundle)
    // Output
    val out = Output(new BTBPred)
  })

  io.in.pc.ready := true.B
  val fireLatch = RegNext(io.in.pc.fire())
  val nextFire = Wire(Bool())
  nextFire := fireLatch



  val btbAddr = new TableAddr(log2Up(BtbSize), BtbBanks)

  // SRAMs to store BTB meta & data
  val btbMeta = List.fill(BtbBanks)(
    Module(new SRAMTemplate(btbMetaEntry(), set = BtbSize / BtbBanks, shouldReset = true, holdRead = true)))
  val btbData = List.fill(BtbBanks)(
    Module(new SRAMTemplate(btbDataEntry(), set = BtbSize / BtbBanks, shouldReset = true, holdRead = true)))

  // BTB read requests
  val baseBank = btbAddr.getBank(io.in.pc.bits)
  // val baseTag = btbAddr.getTag(io.in.pc.bits)
  // val isAligned = baseBank === 0.U
  // circular shifting
  def circularShiftLeft(source: UInt, len: Int, shamt: UInt): UInt = {
    val res = Wire(UInt(len.W))
    val higher = source << shamt
    val lower = source >> (len.U - shamt)
    res := higher | lower
    res
  }
  val realMask = circularShiftLeft(io.in.mask, BtbBanks, baseBank)
  
  // those banks whose indexes are less than baseBank are in the next row
  val isInNextRow = VecInit((0 until BtbBanks).map(_.U < baseBank))

  val baseRow = btbAddr.getBankIdx(io.in.pc.bits)
  // this row is the last row of a bank
  val nextRowStartsUp = baseRow.andR
  val realRow = VecInit((0 until BtbBanks).map(b => Mux(isInNextRow(b.U), Mux(nextRowStartsUp, 0.U, baseRow+1.U), baseRow)))
  val realRowLatch = VecInit(realRow.map(RegNext(_)))

  for (b <- 0 until BtbBanks) {
    btbMeta(b).reset := reset.asBool
    btbMeta(b).io.r.req.valid := realMask(b) && io.in.pc.valid
    btbMeta(b).io.r.req.bits.setIdx := realRow(b)
    btbData(b).reset := reset.asBool
    btbData(b).io.r.req.valid := realMask(b) && io.in.pc.valid
    btbData(b).io.r.req.bits.setIdx := realRow(b)
  }


  // // latch pc for 1 cycle latency when reading SRAM
  // val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  // Entries read from SRAM
  val metaRead = Wire(Vec(BtbBanks, btbMetaEntry()))
  val dataRead = Wire(Vec(BtbBanks, btbDataEntry()))
  val readFire = Wire(Vec(BtbBanks, Bool()))
  for (b <- 0 until BtbBanks) {
    readFire(b) := btbMeta(b).io.r.req.fire() && btbData(b).io.r.req.fire()
    metaRead(b) := btbMeta(b).io.r.resp.data(0)
    dataRead(b) := btbData(b).io.r.resp.data(0)
  }

  val baseBankLatch = btbAddr.getBank(io.in.pcLatch)
  // val isAlignedLatch = baseBankLatch === 0.U
  val baseTag = btbAddr.getTag(io.in.pcLatch)
  // If the next row starts up, the tag needs to be incremented as well
  val tagIncremented = VecInit((0 until BtbBanks).map(b => RegEnable(isInNextRow(b.U) && nextRowStartsUp, io.in.pc.valid)))

  val bankHits = Wire(Vec(BtbBanks, Bool()))
  for (b <- 0 until BtbBanks) {
    bankHits(b) := metaRead(b).valid && 
      (Mux(tagIncremented(b), baseTag+1.U, baseTag) === metaRead(b).tag) && !io.flush && RegNext(readFire(b), init = false.B)
  }

  // taken branches of jumps from a valid entry
  val predTakens = Wire(Vec(BtbBanks, Bool()))
  // not taken branches from a valid entry
  val notTakenBranches = Wire(Vec(BtbBanks, Bool()))
  for (b <- 0 until BtbBanks) {
    predTakens(b) := bankHits(b) && (dataRead(b)._type === BTBtype.J || dataRead(b)._type === BTBtype.B && dataRead(b).pred(1).asBool)
    notTakenBranches(b) := bankHits(b) && dataRead(b)._type === BTBtype.B && !dataRead(b).pred(1).asBool
  }

  // e.g: baseBank == 5 => (5, 6,..., 15, 0, 1, 2, 3, 4)
  val bankIdxInOrder = VecInit((0 until BtbBanks).map(b => (baseBankLatch + b.U) % BtbBanks.U))
  

  // Let predTakens(0) be in correspond with the first instruction in fetchPC
  // val predUInt = predTakens.asUInt
  // val realPreds = Mux(isAlignedLatch, predUInt, Cat(predUInt(BtbBanks-baseBankLatch-1, 0), predUInt(BtbBanks-1, BtbBanks-baseBankLatch))
  // val realPredsVec = VecInit((0 until BtbBanks).map(realPreds(_).asBool))
  // val ntbUInt = notTakenBranches.asUInt
  // val realNtb = Mux(isAlignedLatch, ntbUInt, Cat(ntbUInt(BtbBanks-baseBankLatch-1, 0), ntbUInt(BtbBanks-1, BtbBanks-baseBankLatch))
  // val realNtbVec = VecInit((0 until BtbBanks).map(realNtb(_).asBool))

  val isTaken       = predTakens.reduce(_||_)
  // Priority mux which corresponds with inst orders
  // BTB only produce one single prediction
  val takenTarget = MuxCase(0.U, bankIdxInOrder.map(b => (predTakens(b), dataRead(b).target)))
  val takenType   = MuxCase(0.U, bankIdxInOrder.map(b => (predTakens(b), dataRead(b)._type)))
  // Record which inst is predicted taken
  val takenIdx = MuxCase(0.U, (0 until BtbBanks).map(b => (predTakens(bankIdxInOrder(b)), b.U)))

  // Update logic
  // 1 calculate new 2-bit saturated counter value
  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken = old === ((1 << len)-1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len)-1-1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  val u = io.update
  val newCtr = Mux(!u.hit, "b10".U, satUpdate(u.oldCtr, 2, u.taken))

  val updateOnSaturated = u.taken && u.oldCtr === "b11".U || !u.taken && u.oldCtr === "b00".U

  // 2 write btb
  val updateBankIdx = btbAddr.getBank(u.pc)
  val updateRow = btbAddr.getBankIdx(u.pc)
  val btbMetaWrite = Wire(btbMetaEntry())
  btbMetaWrite.valid := true.B
  btbMetaWrite.tag := btbAddr.getTag(u.pc)
  val btbDataWrite = Wire(btbDataEntry())
  btbDataWrite.target := u.target
  btbDataWrite.pred := newCtr
  btbDataWrite._type := u._type
  btbDataWrite.isRVC := u.isRVC

  val isBr = u._type === BTBtype.B
  val isJ = u._type === BTBtype.J
  val notBrOrJ = u._type =/= BTBtype.B && u._type =/= BTBtype.J

  // Do not update BTB on indirect or return, or correctly predicted J or saturated counters
  val noNeedToUpdate = (!u.misPred && (isBr && updateOnSaturated || isJ)) || (u.misPred && notBrOrJ)

  // do not update on saturated ctrs
  val btbWriteValid = io.redirectValid && !noNeedToUpdate

  for (b <- 0 until BtbBanks) {
    btbMeta(b).io.w.req.valid := btbWriteValid && b.U === updateBankIdx
    btbMeta(b).io.w.req.bits.setIdx := updateRow
    btbMeta(b).io.w.req.bits.data := btbMetaWrite
    btbData(b).io.w.req.valid := btbWriteValid && b.U === updateBankIdx
    btbData(b).io.w.req.bits.setIdx := updateRow
    btbData(b).io.w.req.bits.data := btbDataWrite
  }

  io.out.hit := bankHits.reduce(_||_)
  io.out.taken := isTaken
  io.out.takenIdx := takenIdx(log2Up(PredictWidth)-1, 1)
  io.out.target := takenTarget
  // io.out.writeWay := writeWay
  io.out.notTakens := VecInit((0 until BtbBanks by 2).map(b => notTakenBranches(bankIdxInOrder(b))))
  io.out.dEntries := VecInit((0 until BtbBanks by 2).map(b => dataRead(bankIdxInOrder(b))))
  io.out.hits := VecInit((0 until BtbBanks by 2).map(b => bankHits(bankIdxInOrder(b))))

  XSDebug(io.in.pc.fire(), "[BTB]read: pc=0x%x, baseBank=%d, realMask=%b\n", io.in.pc.bits, baseBank, realMask)
  XSDebug(nextFire, "[BTB]read_resp: pc=0x%x, readIdx=%d-------------------------------\n",
    io.in.pcLatch, btbAddr.getIdx(io.in.pcLatch))
  for (i <- 0 until BtbBanks){
    XSDebug(nextFire, "[BTB]read_resp[b=%d][r=%d]: valid=%d, tag=0x%x, target=0x%x, type=%d, ctr=%d\n",
    i.U, realRowLatch(i), metaRead(i).valid, metaRead(i).tag, dataRead(i).target, dataRead(i)._type, dataRead(i).pred)
  }
  XSDebug(nextFire, "[BTB]bankIdxInOrder:")
  for (i <- 0 until BtbBanks){ XSDebug(nextFire, "%d ", bankIdxInOrder(i))}
  XSDebug(nextFire, "\n")
  XSDebug(io.redirectValid, "[BTB]update_req: pc=0x%x, hit=%d, misPred=%d, oldCtr=%d, taken=%d, target=0x%x, _type=%d\n",
    u.pc, u.hit, u.misPred, u.oldCtr, u.taken, u.target, u._type)
  XSDebug(io.redirectValid, "[BTB]update: noNeedToUpdate=%d, writeValid=%d, bank=%d, row=%d, newCtr=%d\n",
    noNeedToUpdate, btbWriteValid, updateBankIdx, updateRow, newCtr)
}