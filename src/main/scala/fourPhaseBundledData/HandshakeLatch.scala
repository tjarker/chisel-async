package fourPhaseBundledData

import util.Helper._
import chisel3._
import primitives.{CElement, LatchNext}
import primitives.SimulationDelay.SimulationDelayer
import handshake._


class HandshakeLatch[T <: Data](gen: T, init: HandshakeInitializer[T]) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(gen))
    val out = Handshake(gen)
  })

  val latchIn = CElement(init.isValid.B)(
    io.in.req,
    !io.out.ack.addSimulationDelay(1)
  )(reset.asAsyncReset).addSimulationDelay(2)

  io.in.ack := latchIn
  io.out.expand(
    _.req := latchIn,
    _.payload := LatchNext(io.in.payload, init.getInitOption, latchIn)(reset.asAsyncReset)
  )

  def ||>(that: HandshakeLatch[T]): HandshakeLatch[T] = {
    this.io.out <> that.io.in
    that
  }
  def <||(that: HandshakeLatch[T]): HandshakeLatch[T] = {
    this.io.in <> that.io.out
    that
  }
}

object HandshakeLatch {
  def apply[T <: Data](gen: T, init: HandshakeInitializer[T]): HandshakeLatch[T] = Module(new HandshakeLatch(gen, init))

}

