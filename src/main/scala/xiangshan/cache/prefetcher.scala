package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import utils._

abstract class Prefetcher extends DCacheModule {
  val io = IO(new Bundle {
    // prefetch only needs to read meta array
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_resp = Input(Vec(nWays, new L1Metadata))
    // prefetch req to MissQueue
    val req = DecoupledIO(new MissReq)
    val resp = Flipped(ValidIO(new MissResp))
    val finish = DecoupledIO(new MissFinish)
  })
}

class FakePrefetcher extends Prefetcher {
  io.meta_read.valid := false.B
  io.meta_read.bits := DontCare
  io.req.valid := false.B
  io.req.bits := DontCare
  io.finish.valid := false.B
  io.finish.bits := DontCare
}

class NextLinePrefetcher extends Prefetcher {
  
}