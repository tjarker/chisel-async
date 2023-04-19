package async.blocks

import async._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.Hardware.ToggleReg

class HandshakeRegister[T <: Data](gen: T, init: HandshakeInitializer[T]) extends Module {


  val in = IO(HandshakeIn(gen))
  val out = IO(HandshakeOut(gen))

  val click = (in.req =/= in.ack && out.req === out.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {

    in.ack := ToggleReg.init(0.B)
    out.req := ToggleReg.init(init.hasToken.B)
    out.data := (init match {
      case Empty => RegNext(in.data, 0.U.asTypeOf(gen))
      case Token(value) => RegNext(in.data, value)
    })

  }

}

object HandshakeRegister {
  def apply[T <: Data](gen: T, init: HandshakeInitializer[T]): HandshakeRegister[T] = Module(new HandshakeRegister(gen, init))
}

object HandshakeRegisterNext {
  def apply[T <: Data](next: Handshake[T], init: HandshakeInitializer[T]): Handshake[T] = {
    val handshakeRegister = HandshakeRegister(chiselTypeOf(next.data), init)
    handshakeRegister.in <> next
    handshakeRegister.out
  }
}