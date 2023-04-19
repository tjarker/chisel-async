package async.blocks

import async._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.Hardware.ToggleReg

class Sink[T <: Data](gen: T) extends Module {

  val in = IO(HandshakeIn(gen))

  val click = (in.req =/= in.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    in.ack := ToggleReg.init(0.B)
  }

}

object Sink {
  def apply[T <: Data](in: Handshake[T]): Unit = Module(new Sink(chiselTypeOf(in.data))).in <> in
}
