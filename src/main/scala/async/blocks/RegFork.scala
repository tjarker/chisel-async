package async.blocks

import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import async.{Empty, Handshake, HandshakeIn, HandshakeInitializer, HandshakeOut, Token}
import chisel3.{RegNext, withClockAndReset}
import helpers.Hardware.ToggleReg
import helpers.Types.Pair

class RegFork[T <: Data](gen: T, init: HandshakeInitializer[T]) extends Module {


  val io = IO(new Bundle {
    val in = HandshakeIn(gen)
    val out = Pair(HandshakeOut(gen), HandshakeOut(gen))
  })

  val click = (io.in.req =/= io.in.ack && io.out._1.req === io.out._1.ack && io.out._2.req === io.out._2.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {

    io.in.ack := ToggleReg.init(0.B)
    val phaseOut = ToggleReg.init(init.hasToken.B)
    io.out._1.req := phaseOut
    io.out._2.req := phaseOut
    val reg = init match {
      case Empty => RegNext(io.in.data, 0.U.asTypeOf(gen))
      case Token(value) => RegNext(io.in.data, value)
    }
    io.out._1.data := reg
    io.out._2.data := reg

  }

}

object RegFork {
  def apply[T <: Data](gen: T, init: HandshakeInitializer[T]): RegFork[T] = {
    Module(new RegFork(gen, init))
  }

}

object RegForkNext {
  def apply[T <: Data](in: Handshake[T], init: HandshakeInitializer[T]): Pair[Handshake[T], Handshake[T]] = {
    val regfork = RegFork(chiselTypeOf(in.data), init)
    regfork.io.in <> in
    regfork.io.out
  }
}