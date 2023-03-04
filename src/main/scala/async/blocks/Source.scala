package async.blocks

import async.Helper.ToggleReg
import async._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
class Source[T <: Data](token: T) extends Module {

  val o = IO(Handshake(chiselTypeOf(token)))

  private val click = (o.req === o.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    o.req := ToggleReg(1.B)
    o.payload := token
  }

}

object Source {
  def apply[T <: Data](token: T): Handshake[T] = Module(new Source(token)).o
}