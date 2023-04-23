package async.blocks

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util.HasBlackBoxInline

private class SimulationDelay(width: Width, delay: Int) extends BlackBox(Map("w" -> width.get.toInt, "delay" -> delay)) with HasBlackBoxInline {
  val io = IO(new Bundle {
    val in = Input(UInt(width))
    val out = Output(UInt(width))
  })
  setInline(s"SimulationDelay.v",
    s"""module SimulationDelay #(
       | parameter w = 1,
       | parameter delay = 0
       |)(
       | input [w - 1:0] in,
       | output reg [w - 1:0] out = 0
       |);
       |always @(*) begin
       | out <= #delay in;
       |end
       |endmodule""".stripMargin)
}

object SimulationDelay {
  def apply[T <: Data](x: T, delay: Int): T = {
    val m = Module(new SimulationDelay(x.getWidth.W, delay))
    m.io.in := x.asUInt
    m.io.out.asTypeOf(x)
  }

  implicit class SimulationDelayer[T <: Data](x: T) {
    def addSimulationDelay(delay: Int): T = SimulationDelay(x, delay)
  }

}