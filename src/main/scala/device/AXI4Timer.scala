package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.axi4._
import utils._

class TimerIO extends Bundle {
  val mtip = Output(Bool())
}

class AXI4Timer(sim: Boolean = false) extends AXI4SlaveModule(new AXI4Lite, new TimerIO) {
  val mtime = RegInit(0.U(64.W))  // unit: us
  val mtimecmp = RegInit(0.U(64.W))

  val clk = (if (!sim) 40 /* 40MHz / 1000000 */ else 10000)
  val freq = RegInit(clk.U(16.W))
  val inc = RegInit(1000.U(16.W))

  val cnt = RegInit(0.U(16.W))
  val nextCnt = cnt + 1.U
  cnt := Mux(nextCnt < freq, nextCnt, 0.U)
  val tick = (nextCnt === freq)
  when (tick) { mtime := mtime + inc }

  if (sim) {
    val isWFI = WireInit(false.B)
    BoringUtils.addSink(isWFI, "isWFI")
    when (isWFI) { mtime := mtime + 100000.U }
  }

  val mapping = Map(
    RegMap(0x4000, mtimecmp),
    RegMap(0x8000, freq),
    RegMap(0x8008, inc),
    RegMap(0xbff8, mtime)
  )
  def getOffset(addr: UInt) = addr(15,0)

  RegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
    getOffset(waddr), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

  io.extra.get.mtip := RegNext(mtime >= mtimecmp)
}
