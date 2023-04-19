package async.blocks

import async.blocks.SimulationDelay.SimulationDelayer
import async.{Handshake, HandshakeIn, HandshakeOut}
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.Types.Pair

class Merge[T <: Data](gen: T) extends Module {

  val io = IO(new Bundle {
    val in = Pair(HandshakeIn(gen), HandshakeIn(gen))
    val out = HandshakeOut(gen)
  })

  val clickIn = (io.out.req === io.out.ack).addSimulationDelay(1)
  val select1 = io.in._1.req =/= io.in._1.ack
  val select2 = io.in._2.req =/= io.in._2.ack
  val clickOut = (select1 || select2).addSimulationDelay(1)

  withClockAndReset(clickIn.asClock, reset.asAsyncReset) {
    io.in._1.ack := RegNext(io.in._1.req, 0.B)
    io.in._2.ack := RegNext(io.in._2.req, 0.B)
  }

  withClockAndReset(clickOut.asClock, reset.asAsyncReset) {
    io.out.req := ToggleReg.init(0.B)
    io.out.data := Mux(select1, io.in._1.data, io.in._2.data)
  }


}


object Merge {
  def apply[T <: Data](in: Pair[Handshake[T], Handshake[T]]): Handshake[T] = {
    val merge = Module(new Merge(chiselTypeOf(in._1.data)))
    merge.io.in <> in
    merge.io.out
  }
}