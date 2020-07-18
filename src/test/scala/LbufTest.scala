// libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.2.1"

import chisel3._
import chiseltest._
import org.scalatest._
import xiangshan.frontend.LoopBuffer
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import xiangshan.testutils.HasPartialDecoupledDriver
import xiangshan.{FetchPacket, XSBundle}

import scala.util.Random

class BrBundle extends XSBundle {
  val offset1 = UInt(6.W)
  val offset2 = UInt(5.W)
  val offset3 = UInt(9.W)
  val offset4 = UInt(5.W)
  val offset5 = UInt(7.W)
}

class LbufTest extends FlatSpec with ChiselScalatestTester with HasPartialDecoupledDriver{
  def isJal(inst: UInt): Boolean = {
    inst == BitPat("b????????????????????_?????_1101111") || inst == BitPat("b???????_?????_?????_???_?????_1100011")
    //    false.B
  }

  val TEST_SIZE = 20

  var random_case = Vector(
    scala.util.Random.nextInt(858934591).U,
    scala.util.Random.nextInt(858934591).U,
    scala.util.Random.nextInt(858934591).U,
    scala.util.Random.nextInt(858934591).U,
    scala.util.Random.nextInt(858934591).U,
    scala.util.Random.nextInt(858934591).U,
    scala.util.Random.nextInt(858934591).U,
    scala.util.Random.nextInt(858934591).U)

  val case1_instrs = Vector(
    "b00001000100111011101000110111011".U,
    "b00100000001001010011000010000011".U,
    "b00010011010010111011110000001101".U,
    "b00001101001010010111010001111000".U,
    "b00110010110101000001110111110111".U,
    "b00001011011100000010011100100101".U,
    "b00000110100110110101010101110110".U,
    "b00000110101101000011100000100001".U)

  val case1_mask = "b1111111111111111".U
  var case1_pc = 0

  def genBrPacket(bridx: Int = -1): List[UInt] = {
    var brPacket = List[UInt]()
    var inst = Random.nextInt(0x3fffffff).U
    for(i <- 7 to 0 by -1) {
      if(i == bridx) {
        brPacket = List("b111111_11011_111111111001001101111".U) ++ brPacket
      } else {
        while(isJal(inst)) {
          inst = Random.nextInt(0x3fffffff).U
        }
        brPacket = List(inst) ++ brPacket
      }
    }
    brPacket
  }

  it should "test Lbuf" in {
    test(new LoopBuffer) { c =>

      for(cyc <- 0 until 20) {
        println(s"\n==========Cycle$cyc==========\n")
        for(i <- c.io.in.bits.instrs.indices) {
          val fp = genBrPacket()
          c.io.in.bits.instrs(i).poke(fp(i))
        }
        c.io.redirect.poke(false.B)

        for(i <- 0 until 6) {
          c.io.out(i).ready.poke(true.B)
        }

        c.io.in.valid.poke(true.B)
        c.io.in.bits.mask.poke(case1_mask)
        c.io.in.bits.pc.poke(case1_pc.U(39.W))
        println(s"This cycle PC=${case1_pc}\n")
        case1_pc = case1_pc + 32

        c.clock.step()
      }

      println("<<<<<<<<<<<<<<<<<Test finished>>>>>>>>>>>>>>>>")
    }
  }

  it should "Loop Emulate" in {
    test(new LoopBuffer) { c =>
      for(i <- 0 until 6) {
        c.io.out(i).ready.poke(true.B)
      }
      c.io.in.valid.poke(true.B)
      c.io.in.bits.mask.poke(case1_mask)

      // cycle 1
      println(s"\n==========Cycle1==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(7).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(0.U)

      c.clock.step()

      // cycle 2
      println(s"\n==========Cycle2==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 3
      println(s"\n==========Cycle3==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 4
      println(s"\n==========Cycle4==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 5
      println(s"\n==========Cycle5==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 6
      println(s"\n==========Cycle6==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 7
      println(s"\n==========Cycle7==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 8
      println(s"\n==========Cycle8==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
//      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 9
      println(s"\n==========Cycle9==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      //      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 10
      println(s"\n==========Cycle10==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      //      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111110000".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()
    }
  }

  it should "Loop Emulate with a long loop body" in {
    test(new LoopBuffer) { c =>
      for(i <- 0 until 6) {
        c.io.out(i).ready.poke(true.B)
      }
      c.io.in.valid.poke(true.B)
      c.io.in.bits.mask.poke(case1_mask)

      // cycle 1
      println(s"\n==========Cycle1==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
//      c.io.in.bits.instrs(7).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(0.U)

      c.clock.step()

      // cycle 2
      println(s"\n==========Cycle2==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
//      c.io.in.bits.instrs(4).poke("b1111_1000100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(32.U)

      c.clock.step()

      // cycle 3
      println(s"\n==========Cycle3==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(1).poke("b1111_1000100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111000000000000".U)

      c.io.in.bits.pc.poke(64.U)

      c.clock.step()

      // cycle 4
      println(s"\n==========Cycle4==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
//      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 5
      println(s"\n==========Cycle5==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(6).poke("b1111_1000100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111111100".U)

      c.io.in.bits.pc.poke(44.U)

      c.clock.step()

      // cycle 6
      println(s"\n==========Cycle6==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
//      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 7
      println(s"\n==========Cycle7==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      //      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 8
      println(s"\n==========Cycle8==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      //      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 9
      println(s"\n==========Cycle9==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      //      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 10
      println(s"\n==========Cycle10==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      //      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()

      // cycle 11
      println(s"\n==========Cycle11==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      //      c.io.in.bits.instrs(4).poke("b1111_1101100_111111111001001101111".U)

      c.io.redirect.poke(false.B)
      c.io.in.bits.mask.poke("b1111111111111111".U)

      c.io.in.bits.pc.poke(12.U)

      c.clock.step()
    }
  }

  it should "Loop Emulate with a super long loop body" in {
    test(new LoopBuffer) { c =>
      for(i <- 0 until 6) {
        c.io.out(i).ready.poke(true.B)
      }
      c.io.in.valid.poke(true.B)
      c.io.in.bits.mask.poke(case1_mask)

      // cycle 0-7
      for(cyc <- 0 to 7) {
        println(s"\n==========Cycle$cyc==========\n")
        for(i <- c.io.in.bits.instrs.indices) {
          val fp = genBrPacket()
          c.io.in.bits.instrs(i).poke(fp(i))
        }
        c.io.redirect.poke(false.B)
        for(i <- 0 to 7) {c.io.in.bits.mask.poke("b1111111111111111".U)}

        c.io.in.bits.pc.poke((cyc*32).U)

        c.clock.step()
      }

      // cycle 8
      println(s"\n==========Cycle8==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(3).poke("b1111_0000010_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b0000000011111111".U)

      c.io.in.bits.pc.poke(256.U)

      c.clock.step()

      // cycle 9-15
      for(cyc <- 9 to 15) {
        println(s"\n==========Cycle$cyc==========\n")
        for(i <- c.io.in.bits.instrs.indices) {
          val fp = genBrPacket()
          c.io.in.bits.instrs(i).poke(fp(i))
        }

//        if(cyc == 14){
//          c.io.in.bits.instrs(1).poke("b00000_00000_00000_00000_00000_1101111".U)
//          c.io.redirect.poke(true.B)
//          c.io.in.bits.mask.poke("b1111000000000000".U)
//        }else {
//          c.io.redirect.poke(false.B)
//          c.io.in.bits.mask.poke("b1111111111111111".U)
//        }
        c.io.redirect.poke(false.B)
        c.io.in.bits.mask.poke("b1111111111111111".U)

        c.io.in.bits.pc.poke(((cyc-9)*32).U)

        c.clock.step()
      }

      // cycle 16
      println(s"\n==========Cycle16==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(6).poke("b1111_0000010_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b0011111111111111".U)

      c.io.in.bits.pc.poke(244.U)

      c.clock.step()

      // cycle 17-60
      for(cyc <- 17 to 60) {
        println(s"\n==========Cycle$cyc==========\n")
        for(i <- c.io.in.bits.instrs.indices) {
          val fp = genBrPacket()
          c.io.in.bits.instrs(i).poke(fp(i))
        }
        c.io.redirect.poke(false.B)
        c.io.in.bits.mask.poke("b1111111111111111".U)

        c.io.in.bits.pc.poke(((cyc-17)*32).U)

        c.clock.step()
      }
    }
  }

  it should "Loop Emulate with a short loop body" in {
    test(new LoopBuffer) { c =>
      for(i <- 0 until 6) {
        c.io.out(i).ready.poke(true.B)
      }
      c.io.in.valid.poke(true.B)
      c.io.in.bits.mask.poke(case1_mask)

      // cycle 1-6
      for(cyc <- 1 to 6) {
        println(s"\n==========Cycle$cyc==========\n")
        for(i <- c.io.in.bits.instrs.indices) {
          val fp = genBrPacket()
          c.io.in.bits.instrs(i).poke(fp(i))
        }
        c.io.redirect.poke(false.B)
        c.io.in.bits.mask.poke("b1111111111111111".U)

        c.io.in.bits.pc.poke((cyc*32).U)

        c.clock.step()
      }

      // cycle 7
      println(s"\n==========Cycle7==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(1).poke("b1111_1001000_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111000000000000".U)

      c.io.in.bits.pc.poke(224.U)

      c.clock.step()

      // cycle 8-14
      for(cyc <- 8 to 14) {
        println(s"\n==========Cycle$cyc==========\n")
        for(i <- c.io.in.bits.instrs.indices) {
          val fp = genBrPacket()
          c.io.in.bits.instrs(i).poke(fp(i))
        }
        c.io.redirect.poke(false.B)
        c.io.in.bits.mask.poke("b1111111111111111".U)

        c.io.in.bits.pc.poke(((cyc-8)*32).U)

        c.clock.step()
      }

      // cycle 15
      println(s"\n==========Cycle15==========\n")
      for(i <- c.io.in.bits.instrs.indices) {
        val fp = genBrPacket()
        c.io.in.bits.instrs(i).poke(fp(i))
      }
      c.io.in.bits.instrs(1).poke("b1111_1001000_111111111001001101111".U)

      c.io.redirect.poke(true.B)
      c.io.in.bits.mask.poke("b1111000000000000".U)

      c.io.in.bits.pc.poke(224.U)

      c.clock.step()

      // cycle 16-25
      for(cyc <- 16 to 55) {
        println(s"\n==========Cycle$cyc==========\n")
        for(i <- c.io.in.bits.instrs.indices) {
          val fp = genBrPacket()
          c.io.in.bits.instrs(i).poke(fp(i))
        }
        c.io.redirect.poke(false.B)
        c.io.in.bits.mask.poke("b1111111111111111".U)

        c.io.in.bits.pc.poke(((cyc-16)*32).U)

        c.clock.step()
      }
    }
  }
}
