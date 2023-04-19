package async.blocks

import async.blocks.SimulationDelay.SimulationDelayer
import async.{Handshake, HandshakeIn, HandshakeOut}
import chisel3._
import helpers.Hardware.ToggleReg
import helpers._

class HMux[T <: Data](gen: T) extends Module {

  val io = IO(new Bundle {
    val sel = HandshakeIn(Bool())
    val a = HandshakeIn(gen)
    val b = HandshakeIn(gen)
    val out = HandshakeOut(gen)
  })

  val clickIn = (io.out.req === io.out.ack).addSimulationDelay(1)
  val clickOut = ((io.a.req =/= io.a.ack && io.sel.req =/= io.sel.ack && io.sel.data) ||
    (io.b.req =/= io.b.ack && io.sel.req =/= io.sel.ack && !io.sel.data)).addSimulationDelay(1)

  withClockAndReset(clickIn.asClock, reset.asAsyncReset) {
    io.a.ack := ToggleReg.init(0.B, event = io.sel.data)
    io.b.ack := ToggleReg.init(0.B, event = !io.sel.data)
    io.sel.ack := ToggleReg.init(0.B)
  }

  withClockAndReset(clickOut.asClock, reset.asAsyncReset) {
    io.out.req := ToggleReg.init(0.B)
    io.out.data := Mux(io.sel.data, io.a.data, io.b.data)
  }
}

object HMux {
  def apply[T <: Data](sel: Handshake[Bool], a: Handshake[T], b: Handshake[T]): Handshake[T] = {
    val mux = Module(new HMux(chiselTypeOf(a.data)))
    mux.io.expand(
      _.a <> a,
      _.b <> b,
      _.sel <> sel
    )
    mux.io.out
  }
}
