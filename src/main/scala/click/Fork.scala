package click

import util.Helper._
import chisel3._
import handshake._
import primitives.SimulationDelay.SimulationDelayer

class Fork[T <: Data](n: Int, gen: T) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(gen))
    val out = Vec(n, Handshake(gen))
  })

  val click = io.out.map(_.ack =/= io.in.ack).reduce(_ && _).addSimulationDelay(2)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    io.in.ack := ToggleReg(0.B)
    io.out.foreach { o =>
      o.req := io.in.req
      o.payload := io.in.payload
    }
  }

}

object Fork {
  def apply[T <: Data](n: Int, gen: T): Fork[T] = Module(new Fork(n, gen))
  def apply[T <: Data](in: Handshake[T])(n: Int): Vec[Handshake[T]] = {
    val fork = Fork(n, chiselTypeOf(in.payload))
    fork.io.in <> in
    fork.io.out
  }
}