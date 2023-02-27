package click

import util.Helper._
import chisel3._
import handshake.Handshake

class Barrier[T <: Data](gen:  T) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(gen))
    val out = Handshake(gen)
    val enable = Input(Bool())
  })

  io.in <> io.out
  io.out.req := io.in.req && io.enable

}

object Barrier {
  def apply[T <: Data](in: Handshake[T], en: Bool): Handshake[T] = {
    val barrier = Module(new Barrier(chiselTypeOf(in.payload)))
    barrier.io.expand(
      _.in <> in,
      _.enable := en
    )
    barrier.io.out
  }
}
