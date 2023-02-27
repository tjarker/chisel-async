package primitives

import util.Helper._
import chisel3._
import chisel3.experimental.RawParam
import chisel3.util.HasBlackBoxInline


class LatchBlackBox[T <: Data](gen: T, init: Option[T]) extends BlackBox(Map(
  "w" -> gen.getWidth,
  "init" -> RawParam(s"${gen.getWidth}'h" + init.andThen(_.litValue.toString(16)).getOrElse("0"))
)) with HasBlackBoxInline {
  val io = IO(new Bundle {
    val in = Input(UInt(gen.getWidth.W))
    val reset = Input(AsyncReset())
    val en = Input(Bool())
    val out = Output(UInt(gen.getWidth.W))
  })
  setInline(s"LatchBlackBox.v",
    s"""module LatchBlackBox #(
       | parameter w = 1,
       | parameter init = 0
       |)(
       | input [w - 1:0] in,
       | input en,
       | input reset,
       | output reg [w - 1:0] out
       |);
       |always @(en or in or reset) begin
       |  if(reset) out <= init;
       |  else if(!en) out <= in;
       |end
       |initial begin
       | out <= init;
       |end
       |endmodule""".stripMargin)
}

class Latch[T <: Data](gen: T, init: Option[T]) extends RawModule {
  val io = IO(new Bundle {
    val in = Input(gen)
    val reset = Input(AsyncReset())
    val en = Input(Bool())
    val out = Output(gen)
  })
  val latch = Module(new LatchBlackBox(gen, init))
  latch.io.expand(_.in := io.in.asUInt, _.en := io.en, _.reset := io.reset)
  io.out := latch.io.out.asTypeOf(gen)
}

object Latch {
  def apply[T <: Data](gen: T)(implicit reset: AsyncReset): Latch[T] = {
    val latch = Module(new Latch(gen, None))
    latch.io.reset := reset
    latch
  }
}

object LatchInit {
  def apply[T <: Data](gen: T, init: T)(implicit reset: AsyncReset): Latch[T] = {
    val latch = Module(new Latch(gen, Some(init)))
    latch.io.reset := reset
    latch
  }
  def apply[T <: Data](init: T)(implicit reset: AsyncReset): Latch[T] = LatchInit(chiselTypeOf(init), init)
}
object LatchNext {
  def apply[T <: Data](next: T, init: T, en: Bool)(implicit reset: AsyncReset): T = {
    val latch = LatchInit(init)
    latch.io.expand(_.in := next, _.en := en)
    latch.io.out
  }
  def apply[T <: Data](next: T, init: Option[T], en: Bool)(implicit reset: AsyncReset): T = {
    val latch = Module(new Latch(chiselTypeOf(next), init))
    latch.io.expand(_.in := next, _.en := en, _.reset := reset)
    latch.io.out
  }
  def apply[T <: Data](next: T, en: Bool)(implicit reset: AsyncReset): T = {
    val latch = Latch(chiselTypeOf(next))
    latch.io.expand(_.in := next, _.en := en)
    latch.io.out
  }
}