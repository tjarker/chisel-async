package click

import util.Helper._
import chisel3._
import handshake.HandshakeInitializer.ValidToken
import handshake._
import primitives.SimulationDelay.SimulationDelayer

class HandshakeRegister[T <: Data](gen: T, init: HandshakeInitializer[T]) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(gen))
    val out = Handshake(gen)
  })

  val click = (io.in.req =/= io.in.ack && io.out.req === io.out.ack).addSimulationDelay(2)

  withClockAndReset(click.asClock, reset.asAsyncReset) {

    io.in.ack := ToggleReg(0.B)
    io.out.req := ToggleReg(init.isValid.B)
    io.out.payload := (init match {
      case HandshakeInitializer.Empty => RegNext(io.in.payload)
      case HandshakeInitializer.ValidToken(value) => RegNext(io.in.payload, value)
    })

  }

  def ||>(that: HandshakeRegister[T]): HandshakeRegister[T] = {
    this.io.out <> that.io.in
    that
  }

  def <||(that: HandshakeRegister[T]): HandshakeRegister[T] = {
    this.io.in <> that.io.out
    that
  }

}

object HandshakeRegister {
  def apply[T <: Data](gen: T, init: HandshakeInitializer[T]): HandshakeRegister[T] = Module(new HandshakeRegister(gen, init))
}

object Hallo extends App { emitVerilog(new HandshakeRegister(UInt(8.W), ValidToken(10.U)))}