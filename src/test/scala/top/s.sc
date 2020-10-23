import chisel3._
import chisel3.util._
import scala.math._

def leftOR(x: UInt): UInt = leftOR(x, x.getWidth, x.getWidth)
def leftOR(x: UInt, width: Integer, cap: Integer = 999999): UInt = {
  val stop = min(width, cap)
  def helper(s: Int, x: UInt): UInt =
    if (s >= stop) x else helper(s+s, x | (x << s)(width-1,0))
  helper(1, x)(width-1, 0)
}

class X extends Module {
  val io = IO(new Bundle {})
  print(leftOR("b0001000".U))
}

val a = new X