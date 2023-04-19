package async.blocks

import helpers._
import async._
import chisel3._

private class Barrier[T <: Data](gen:  T) extends Module {

  val io = IO(new Bundle {
    val in = HandshakeIn(gen)
    val out = HandshakeOut(gen)
    val enable = Input(Bool())
  })

  io.in <> io.out
  io.out.req := io.in.req && io.enable

}

object Barrier {
  def apply[T <: Data](in: Handshake[T], en: Bool): Handshake[T] = {
    val barrier = Module(new Barrier(chiselTypeOf(in.data)))
    barrier.io.expand(
      _.in <> in,
      _.enable := en
    )
    barrier.io.out
  }

  implicit class Blocker[T <: Data](x: Handshake[T]) {
    def block(en: Bool): Handshake[T] = Barrier(x, en)
  }
}
