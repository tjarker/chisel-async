package async.blocks

import async.Helper.ToggleReg
import async._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._

class Sink[T <: Data](gen: T) extends Module {

  val i = IO(Flipped(Handshake(gen)))

  private val click = (i.req =/= i.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    i.ack := ToggleReg(0.B)
  }

}

object Sink {
  def apply[T <: Data](in: Handshake[T]): Unit = Module(new Sink(chiselTypeOf(in.data))).i <> in
}
