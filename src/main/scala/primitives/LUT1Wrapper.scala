package primitives

import chisel3._
import chisel3.util.HasBlackBoxInline
import primitives.LUT1Wrapper.truthTableToInit


class LUT1Wrapper(truthTable: Map[Boolean, Boolean]) extends BlackBox(Map(
  "init" -> truthTableToInit(truthTable)
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
  def truthTableToInit(truthTable: Map[Boolean, Boolean]) = (if(truthTable(true)) 2 else 0) | (if(truthTable(false)) 1 else 0)
}