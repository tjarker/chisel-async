package primitives

import util.Helper._
import chisel3._
import chisel3.util.HasBlackBoxInline

class CElement(init: Bool) extends BlackBox(Map(
  "init" -> init.litToBoolean.toInt
)) with HasBlackBoxInline {
  val io = IO(new Bundle {
    val x = Input(Bool())
    val y = Input(Bool())
    val z = Output(Bool())
    val reset = Input(AsyncReset())
  })
  setInline(s"CElement.v",
    s"""module CElement #(
       | parameter init = 0
       |)(
       | input x,
       | input y,
       | output reg z,
       | input reset
       |);
       |always @(*) begin
       | if(reset) z <= init;
       | else if(x & y) z <= 1;
       | else if(!x & !y) z <= 0;
       |end
       |initial begin
       | z <= init;
       |end
       |endmodule""".stripMargin)
}

object CElement {
  def apply(init: Bool)(x: Bool, y: Bool)(implicit reset: AsyncReset): Bool = {
    val c = Module(new CElement(init))
    c.io.expand(_.x := x, _.y := y, _.reset := reset)
    c.io.z
  }

}