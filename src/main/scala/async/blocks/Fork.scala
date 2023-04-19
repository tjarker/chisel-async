package async.blocks

import async._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.BundleExpander
import helpers.Hardware.ToggleReg

private class Fork[T <: Data](n: Int, gen: T) extends Module {

  val io = IO(new Bundle {
    val in = HandshakeIn(gen)
    val out = Vec(n, HandshakeOut(gen))
  })


  val click = io.out.map(_.ack =/= io.in.ack).reduce(_ && _).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    io.in.ack := ToggleReg.init(0.B)
    io.out.foreach { _.expand(
      _.req := io.in.req,
      _.data := io.in.data
    )
    }
  }

}

object Fork {
  def apply[T <: Data](n: Int, in: Handshake[T]): Vec[Handshake[T]] = {
    val fork = Module(new Fork(n, chiselTypeOf(in.data)))
    fork.io.in <> in
    fork.io.out
  }
}