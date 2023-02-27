package fourPhaseBundledData

import chisel3._
import primitives.Delay.BoolDelayer
import primitives.DelayElement
import handshake._

class FunctionBlock[A <: Data, B <: Data](gens: (A,B), fun: A => B, delay: Int)(implicit delayElement: () => DelayElement, sim: Boolean) extends RawModule {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(gens._1))
    val out = Handshake(gens._2)
  })

  io.in.ack := io.out.ack
  io.out.payload := fun(io.in.payload)
  io.out.req := io.in.req.addDelay(delay)

}

object FunctionBlock {
  def apply[A <: Data, B <: Data](gens: (A,B), fun: A => B, delay: Int)(implicit delayElement: () => DelayElement, sim: Boolean): FunctionBlock[A,B] =
    Module(new FunctionBlock(gens, fun, delay))

  def apply[A <: Data, B <: Data](in: Handshake[A], delay: Int)(fun: A => B)(implicit delayElement: () => DelayElement, sim: Boolean): Handshake[B] = {
    val functionBlock = FunctionBlock(chiselTypeOf(in.payload) -> chiselTypeOf(fun(in.payload)), fun, delay)
    functionBlock.io.in <> in
    functionBlock.io.out
  }
}