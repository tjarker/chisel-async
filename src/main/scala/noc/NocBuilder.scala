package noc

import async.{Handshake, HandshakeOut}
import async.blocks.{Sink, Source}
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import helpers.{Mapper2D, OptionExtension}
import helpers.Types.{Coordinate, Grid, GridBuilder}
import noc.Direction._
import noc.NocBuilder.NocInterface
import noc.Position.{EasternEdge, Inside, NorthEasternCorner, NorthWesternCorner, NorthernEdge, SouthEasternCorner, SouthWesternCorner, SouthernEdge, WesternEdge}

object NocBuilder {

  trait NocInterface[P <: Data] {
    val nocIO: Port[P]
  }

  def neighbors[P <: Data](routers: Seq[Seq[Router[P]]], coordinate: Coordinate): Map[Direction, Option[Port[P]]] = {
    val (x,y) = coordinate.toInts
    Map(
      North -> routers.tryGet(x, y + 1).andThen(_.io.south.get),
      NorthEast -> routers.tryGet(x + 1, y + 1).andThen(_.io.southwest.get),
      East -> routers.tryGet(x + 1, y).andThen(_.io.west.get),
      SouthEast -> routers.tryGet(x + 1, y - 1).andThen(_.io.northwest.get),
      South -> routers.tryGet(x, y - 1).andThen(_.io.north.get),
      SouthWest -> routers.tryGet(x - 1, y - 1).andThen(_.io.northeast.get),
      West -> routers.tryGet(x - 1, y).andThen(_.io.east.get),
      NorthWest -> routers.tryGet(x - 1, y + 1).andThen(_.io.southeast.get)
    )
  }

  def apply[P <: Data](p: NocParameters[P], modules: Seq[Seq[RawModule with NocInterface[P]]]): Seq[Seq[Router[P]]] = {

    val routers = Position.coordinateGrid(p.size).zip2d(modules).map2d { case ((position, coordinate), module) =>
      val router = Router(coordinate, position)(p)
      router.io.local.get <> module.nocIO
      router
    }

    routers.map2d { router =>
      val neighbors = NocBuilder.neighbors(routers, router.coordinate)
      router.io.all.filter(_.dir != Local).foreach(port => port <> neighbors(port.dir).get)
    }

    routers
  }

}


class Dummy()(implicit p: NocParameters[UInt]) extends Module with NocInterface[UInt]  {
  override val nocIO: Port[UInt] = IO(LocalPort())

  val sink = Sink(nocIO.inbound)
  val click = nocIO.inbound.req =/= nocIO.inbound.ack
  withClockAndReset(click.asClock, reset.asAsyncReset) {
    val reg = RegNext(nocIO.inbound.data)
    nocIO.outbound.data := reg
  }

  nocIO.outbound.req := 0.B


}

class NocTest extends Module {

  implicit val p = NocParameters(4 by 4, () => UInt(8.W))

  val io = IO(HandshakeOut(Packet()))

  val dummies = (Module(new Sender) +: Seq.fill(3)(Module(new Dummy))) +: Seq.fill(3,4)(Module(new Dummy))

  val routers = NocBuilder(p, dummies)

  io.req := routers(0)(0).io.local.get.outbound.req
  io.data := routers(0)(0).io.local.get.outbound.data

}

object NocTest extends App { emitVerilog(new NocTest) }
