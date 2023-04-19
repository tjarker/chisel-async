package async.blocks

import async._
import async.blocks.Delay.BoolDelayer
import chisel3._
import helpers.BundleExpander


private class FunctionBlock[A <: Data, B <: Data](gens: (A,B), fun: A => B, delay: Int)(implicit delayElementConfig: DelayElementConfig) extends RawModule {

  val io = IO(new Bundle {
    val in = HandshakeIn(gens._1)
    val out = HandshakeOut(gens._2)
  })

  io.in.ack := io.out.ack
  io.out.expand(
    _.req := io.in.req.addDelay(delay),
    _.data := fun(io.in.data)
  )
}

object FunctionBlock {

  def apply[A <: Data, B <: Data](in: Handshake[A], delay: Int)(fun: A => B)(implicit delayElementConfig: DelayElementConfig): Handshake[B] = {
    val functionBlock = Module(new FunctionBlock(chiselTypeOf(in.data) -> chiselTypeOf(fun(in.data)), fun, delay))
    functionBlock.io.in <> in
    functionBlock.io.out
  }

  implicit class FunctionBlockExtension[A <: Data](x: Handshake[A]) {
    def applyFunction[B <: Data](delay: Int)(f: A => B)(implicit delayElementConfig: DelayElementConfig): Handshake[B] = FunctionBlock(x, delay)(f)
  }
}