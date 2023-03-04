package async.primitives

import async.primitives.LUT1Wrapper.genInit
import chisel3._
import chisel3.util.HasBlackBoxInline


class LUT1Wrapper(f0: Bool, f1: Bool) extends BlackBox(Map(
  "init" -> genInit(f0, f1)
)) with HasBlackBoxInline {

  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })

  setInline(s"LUT1Wrapper.v",
    s"""module LUT1Wrapper #(
       | parameter init = 0
       |)(
       | input in,
       | output out
       |);
       |(* DONT_TOUCH = "yes" *) LUT1 #(.INIT(init)) lut (
       |    .I0(in),
       |    .O(out)
       |);
       |endmodule""".stripMargin)

}

object LUT1Wrapper {
  def genInit(f0: Bool, f1: Bool) = (if(f0.litToBoolean) 1 else 0) | (if(f1.litToBoolean) 2 else 0)
}