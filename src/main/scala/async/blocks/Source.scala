package async.blocks

import async.{Handshake, HandshakeOut}
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.Hardware.ToggleReg

class Source[T <: Data](token: T) extends Module {

  val out = IO(HandshakeOut(chiselTypeOf(token)))

  val click = (out.req === out.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    out.req := ToggleReg.init(1.B)
    out.data := token
  }

}

object Source {
  def apply[T <: Data](token: T): Handshake[T] = Module(new Source(token)).out
}