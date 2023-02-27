package primitives

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
      val lut = Module(new LUT1Wrapper(Map(false -> true, true -> false)))
      lut.io.in := io.in
      io.out := lut.io.out
    }

    def apply(): DelayElement = new XilinxLUT1Delay
  }


}
