package async.blocks

import async._
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.{BundleExpander, SeqToChiselVec}
import helpers.Hardware.ToggleReg
import helpers.Types.Pair




object Join {

  def apply[A <: Data, B <: Data](left: Handshake[A], right: Handshake[B]): Handshake[Pair[A,B]] = {
    val join = Module(new Join2(chiselTypeOf(left.data) -> chiselTypeOf(right.data)))
    join.io.in._1 <> left
    join.io.in._2 <> right
    join.io.out
  }
  def apply[A <: Data](x: Handshake[A], xs: Handshake[A]*): Handshake[Vec[A]] = {
    val join = Module(new JoinX(xs.length + 1, chiselTypeOf(x.data)))
    join.io.in <> (x +: xs).toVec
    join.io.out
  }

  private class JoinX[A <: Data](n: Int, gen: A) extends Module {
    val io = IO(new Bundle {
      val in = Vec(n, HandshakeIn(gen))
      val out = HandshakeOut(Vec(n, gen))
    })

    val click = io.in.map(_.req =/= io.out.req).reduce(_ && _).addSimulationDelay(1)

    withClockAndReset(click.asClock, reset.asAsyncReset) {
      io.in.foreach(_.ack := io.out.ack)
      io.out.expand(
        _.req := ToggleReg.init(0.B),
        _.data := io.in.map(_.data).toVec
      )
    }
  }

  private class Join2[A <: Data, B <: Data](gens: (A, B)) extends Module {
    val io = IO(new Bundle {
      val in = Pair(HandshakeIn(gens._1), HandshakeIn(gens._2))
      val out = HandshakeOut(Pair(gens._1, gens._2))
    })


    val click = Seq(io.in._1.req, io.in._2.req).map(_ =/= io.out.req).reduce(_ && _).addSimulationDelay(1)

    withClockAndReset(click.asClock, reset.asAsyncReset) {
      io.in._1.ack := io.out.ack
      io.in._2.ack := io.out.ack
      io.out.expand(
        _.req := ToggleReg.init(0.B),
        _.data := Pair(io.in._1.data, io.in._2.data)
      )
    }
  }

}