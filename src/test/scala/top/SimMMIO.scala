package top

import chisel3._
import chipsalliance.rocketchip.config
import device._
import freechips.rocketchip.amba.axi4.{AXI4MasterParameters, AXI4MasterPortParameters, AXI4SlaveNode, AXI4SlavePortParameters, AXI4ToTL, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLErrorEvaluator, TLMasterParameters, TLXbar}

class SimMMIO()(implicit p: config.Parameters) extends LazyModule {

  val uart = LazyModule(new AXI4UART(AddressSet(0x40600000L, 0xf)))
  val vga = LazyModule(new AXI4VGA(
    sim = false,
    fbAddress = AddressSet(0x50000000L, 0x3fffffL),
    ctrlAddress = AddressSet(0x40001000L, 0x7L)
  ))
  val flash = LazyModule(new AXI4Flash(AddressSet(0x40000000L, 0xfff)))
  val sd = LazyModule(new AXI4DummySD(AddressSet(0x40002000L, 0xfff)))

  val axiBus = AXI4Xbar()

  uart.node := axiBus
  vga.node :*= axiBus
  flash.node := axiBus
  sd.node := axiBus

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle() {
      val uart = new UARTIO
    })
    io.uart <> uart.module.io.extra.get
  }

}


//class SimMMIO(para: TLParameters) extends Module {
//  val io = IO(new Bundle {
//    val rw = Flipped(TLCached(para))
//    val uart = new UARTIO
//  })
//
//  val devAddrSpace = List(
//    (0x40600000L, 0x10L), // uart
//    (0x50000000L, 0x400000L), // vmem
//    (0x40001000L, 0x8L),  // vga ctrl
//    (0x40000000L, 0x1000L),  // flash
//    (0x40002000L, 0x1000L)  // dummy sdcard
//  )
//
//  val xbar = Module(new NaiveTL1toN(devAddrSpace, io.rw.params))
//  xbar.io.in <> io.rw
//
//  val axiOut = xbar.io.out.map(tl => AXI4ToAXI4Lite(MMIOTLToAXI4(tl)))
//
//  val uart = Module(new AXI4UART)
//  val vga = Module(new AXI4VGA(sim = true))
//  val flash = Module(new AXI4Flash)
//  val sd = Module(new AXI4DummySD)
//
//  uart.io.in <> axiOut(0)
//  vga.io.in.fb <> axiOut(1)
//  vga.io.in.ctrl <> axiOut(2)
//  flash.io.in <> axiOut(3)
//  sd.io.in <> axiOut(4)
//  vga.io.vga := DontCare
//  io.uart <> uart.io.extra.get
//}
