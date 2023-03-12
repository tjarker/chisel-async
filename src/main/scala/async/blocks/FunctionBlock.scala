package async.blocks

import async._
import async.blocks.Delay.BoolDelayer
import chisel3._
import helpers.BundleExpander


private class FunctionBlock[A <: Data, B <: Data](gens: (A,B), fun: A => B, delay: Int)(implicit delayElementConfig: DelayElementConfig) extends RawModule {

  val i = IO(Flipped(Handshake(gens._1)))
  val o = IO(Handshake(gens._2))

  i.ack := o.ack
  o.expand(
    _.req := i.req.addDelay(delay),
    _.data := fun(i.data)
  )
}

object FunctionBlock {

  def apply[A <: Data, B <: Data](in: Handshake[A], delay: Int)(fun: A => B)(implicit delayElementConfig: DelayElementConfig): Handshake[B] = {
    val functionBlock = Module(new FunctionBlock(chiselTypeOf(in.data) -> chiselTypeOf(fun(in.data)), fun, delay))
    functionBlock.i <> in
    functionBlock.o
  }

  implicit class FunctionBlockExtension[A <: Data](x: Handshake[A]) {
    def applyFunction[B <: Data](delay: Int)(f: A => B)(implicit delayElementConfig: DelayElementConfig): Handshake[B] = FunctionBlock(x, delay)(f)
  }
}