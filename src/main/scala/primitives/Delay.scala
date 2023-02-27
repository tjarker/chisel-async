package primitives

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util.HasBlackBoxInline

class SimulationDelay(width: Width, delay: Int) extends BlackBox(Map("w" -> width.get.toInt, "delay" -> delay)) with HasBlackBoxInline {
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
       | output reg [w - 1:0] out
       |);
       |always @(*) begin
       | out <= #delay in;
       |end
       |initial begin
       | out <= in;
       |end
       |endmodule""".stripMargin)
}

class Delay(delay: Int, delayElement: () => DelayElement) extends RawModule {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })


  io.out := (0 until (delay + (if(delay % 2 == 0) 0 else 1))).foldLeft(io.in) { (acc, _) =>
    val m = Module(delayElement())
    m.io.in := acc
    m.io.out
  }

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

object Delay {
  def apply(x: Bool, delay: Int)(implicit delayElement: () => DelayElement): Bool = {
    val mod = Module(new Delay(delay, delayElement))
    mod.io.in := x
    mod.io.out
  }

  implicit class BoolDelayer(x: Bool) {
    def addDelay(delay: Int)(implicit delayElement: () => DelayElement, simulation: Boolean): Bool =
      if(simulation) SimulationDelay(x, delay) else Delay(x, delay)
  }

}