package fourPhaseBundledData


import util.Helper._
import chisel3._
import primitives.CElement
import handshake._
import primitives.SimulationDelay.SimulationDelayer

class Fork[T <: Data](n: Int, gen: T) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(gen))
    val out = Vec(n, Handshake(gen))
  })

  io.out.foreach(_ <> io.in)
  io.in.ack := CElement(0.B)(
    io.out.head.ack,
    io.out.tail.map(_.ack).reduce(_ & _)
  )(reset.asAsyncReset).addSimulationDelay(2)

}

object Fork {
  def apply[T <: Data](n: Int, gen: T): Fork[T] = Module(new Fork(n, gen))
  def apply[T <: Data](in: Handshake[T])(n: Int): Vec[Handshake[T]] = {
    val fork = Fork(n, chiselTypeOf(in.payload))
    fork.io.in <> in
    fork.io.out
  }
}
