package async.blocks

import async.blocks.SimulationDelay.SimulationDelayer
import async.{Handshake, HandshakeIn, HandshakeOut}
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.Types.Pair
import helpers._

class Demux[T <: Data](gen: T) extends Module {

  val io = IO(new Bundle {
    val in = HandshakeIn(gen)
    val sel = HandshakeIn(Bool())
    val out = Pair(HandshakeOut(gen), HandshakeOut(gen))
  })

  val clickIn = (io.out._1.req === io.out._1.ack && io.out._2.req === io.out._2.ack).addSimulationDelay(1)
  val clickOut = (io.in.req =/= io.in.ack && io.sel.req =/= io.sel.ack).addSimulationDelay(1)

  withClockAndReset(clickIn.asClock, reset.asAsyncReset) {
    val phaseIn = ToggleReg.init(0.B)
    io.in.ack := phaseIn
    io.sel.ack := phaseIn
  }
  withClockAndReset(clickOut.asClock, reset.asAsyncReset) {
    io.out._1.req := ToggleReg.init(0.B, !io.sel.data)
    io.out._2.req := ToggleReg.init(0.B, io.sel.data)
    io.out._1.data := io.in.data
    io.out._2.data := io.in.data
  }

}

object Demux {
  def apply[T <: Data](sel: Handshake[Bool], in: Handshake[T]): Pair[Handshake[T], Handshake[T]] = {
    val demux = Module(new Demux(chiselTypeOf(in.data)))
    demux.io.expand(
      _.sel <> sel,
      _.in <> in
    )
    demux.io.out
  }
}