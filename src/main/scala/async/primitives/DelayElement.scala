package async.primitives

import chisel3._

abstract class DelayElement extends RawModule {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })
}

object DelayElement {

  object Xilinx {

    class XilinxLUT1Delay extends DelayElement {
      val lut = Module(new LUT1Wrapper(0.B, 1.B))
      lut.io.in := io.in
      io.out := lut.io.out
    }

    def apply(): DelayElement = new XilinxLUT1Delay
  }


}
