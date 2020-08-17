package xiangshan.frontend
import utils.XSInfo
import chisel3._
import chisel3.util._
import utils.PipelineConnect
import xiangshan._


class Frontend extends XSModule {
  val io = IO(new Bundle() {
    val backend = new FrontendToBackendIO
  })

  val ifu = Module(new IFU)
  val fakeicache = Module(new FakeCache)
  val ibuffer = if(EnableLB) Module(new LoopBuffer) else Module(new Ibuffer)
  // val ibuffer = Module(new LoopBuffer)

  val needFlush = io.backend.redirect.valid

  ifu.io.redirect <> io.backend.redirect
  ifu.io.inOrderBrInfo <> io.backend.inOrderBrInfo
  ifu.io.outOfOrderBrInfo <> io.backend.outOfOrderBrInfo
  fakeicache.io.in <> ifu.io.icacheReq
  ifu.io.icacheResp <> fakeicache.io.out
  fakeicache.io.flush := ifu.io.icacheFlush

  ibuffer.io.in <> ifu.io.fetchPacket
  ibuffer.io.flush := needFlush

  io.backend.cfVec <> ibuffer.io.out

  if(EnableLB) {
    ifu.io.inLoop := ibuffer.io.inLoop
    ifu.io.LBredirect <> ibuffer.io.LBredirect
    ifu.io.LBFetch <> ibuffer.io.IFUFetch
  }else {
    ifu.io.inLoop := false.B
    ifu.io.LBredirect.valid := false.B
    ifu.io.LBredirect.bits := DontCare
    ifu.io.LBFetch := DontCare
    ibuffer.io.IFUFetch := DontCare
  }

  for(out <- ibuffer.io.out){
    XSInfo(out.fire(),
      p"inst:${Hexadecimal(out.bits.instr)} pc:${Hexadecimal(out.bits.pc)}\n"
    )
  }


}
