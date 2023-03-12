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
    join.i._1 <> left
    join.i._2 <> right
    join.o
  }
  def apply[A <: Data](x: Handshake[A], xs: Handshake[A]*): Handshake[Vec[A]] = {
    val join = Module(new JoinX(xs.length + 1, chiselTypeOf(x.data)))
    join.i <> (x +: xs).toVec
    join.o
  }

  private class JoinX[A <: Data](n: Int, gen: A) extends Module {
    val i = IO(Vec(n, Flipped(Handshake(gen))))
    val o = IO(Handshake(Vec(n, gen)))

    private val click = i.map(_.req =/= o.req).reduce(_ && _).addSimulationDelay(1)

    withClockAndReset(click.asClock, reset.asAsyncReset) {
      i.foreach(_.ack := o.ack)
      o.expand(
        _.req := ToggleReg(0.B),
        _.data := i.map(_.data).toVec
      )
    }
  }
  private class Join2[A <: Data, B <: Data](gens: (A, B)) extends Module {
    val i = IO(Flipped(Pair(Handshake(gens._1), Handshake(gens._2))))
    val o = IO(Handshake(Pair(gens._1, gens._2)))

    private val click = Seq(i._1.req, i._2.req).map(_ =/= o.req).reduce(_ && _).addSimulationDelay(1)

    withClockAndReset(click.asClock, reset.asAsyncReset) {
      i._1.ack := o.ack
      i._2.ack := o.ack
      o.expand(
        _.req := ToggleReg(0.B),
        _.data := Pair(i._1.data, i._2.data)
      )
    }
  }

}