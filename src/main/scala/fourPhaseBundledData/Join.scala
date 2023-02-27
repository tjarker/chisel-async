package fourPhaseBundledData

import util.Helper._
import chisel3._
import primitives.CElement
import primitives.SimulationDelay.SimulationDelayer
import handshake._


class Join[A <: Data, B <: Data](gens: (A,B)) extends Module {

  val io = IO(new Bundle {
    val left = Flipped(Handshake(gens._1))
    val right = Flipped(Handshake(gens._2))
    val out = Handshake(Pair(gens._1, gens._2))
  })

  io.left.ack := io.out.ack
  io.right.ack := io.out.ack
  io.out.expand(
    _.payload := Pair(io.left.payload, io.right.payload),
    _.req := CElement(0.B)(io.left.req, io.right.req)(reset.asAsyncReset).addSimulationDelay(2)
  )

}


object Join {
  def apply[A <: Data, B <: Data](gens: (A,B)): Join[A,B] = Module(new Join(gens))
  def apply[A <: Data, B <: Data](left: Handshake[A], right: Handshake[B]): Handshake[Pair[A,B]] = {
    val join = Join(chiselTypeOf(left.payload) -> chiselTypeOf(right.payload))
    join.io.expand(
      _.left <> left,
      _.right <> right
    )
    join.io.out
  }
}