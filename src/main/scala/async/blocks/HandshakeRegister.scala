package async.blocks

import async._
import async.Helper._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._

class HandshakeRegister[T <: Data](gen: T, init: HandshakeInitializer[T]) extends Module {


  val i = IO(Flipped(Handshake(gen)))
  val o = IO(Handshake(gen))

  private val click = (i.req =/= i.ack && o.req === o.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {

    i.ack := ToggleReg(0.B)
    o.req := ToggleReg(init.hasToken.B)
    o.payload := (init match {
      case Empty => RegNext(i.payload)
      case Token(value) => RegNext(i.payload, value)
    })

  }

  def ||>(that: HandshakeRegister[T]): HandshakeRegister[T] = {
    this.o <> that.i
    that
  }

  def <||(that: HandshakeRegister[T]): HandshakeRegister[T] = {
    this.i <> that.o
    that
  }

}

object HandshakeRegister {
  def apply[T <: Data](gen: T, init: HandshakeInitializer[T]): HandshakeRegister[T] = Module(new HandshakeRegister(gen, init))
}

object HandshakeRegisterNext {
  def apply[T <: Data](next: Handshake[T], init: HandshakeInitializer[T]): Handshake[T] = {
    val handshakeRegister = HandshakeRegister(chiselTypeOf(next.payload), init)
    handshakeRegister.i <> next
    handshakeRegister.o
  }
}