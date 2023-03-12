package async.blocks

import async._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.BundleExpander
import helpers.Hardware.ToggleReg

private class Fork[T <: Data](n: Int, gen: T) extends Module {

  val i = IO(Flipped(Handshake(gen)))
  val o = IO(Vec(n, Handshake(gen)))

  private val click = o.map(_.ack =/= i.ack).reduce(_ && _).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    i.ack := ToggleReg(0.B)
    o.foreach { _.expand(
      _.req := i.req,
      _.data := i.data
    )
    }
  }

}

object Fork {
  def apply[T <: Data](n: Int, in: Handshake[T]): Vec[Handshake[T]] = {
    val fork = Module(new Fork(n, chiselTypeOf(in.data)))
    fork.i <> in
    fork.o
  }
}