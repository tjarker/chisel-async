package noc

import async.{Handshake, HandshakeOut}
import async.blocks.{Sink, Source}
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import helpers.{Mapper2D, OptionExtension, Zipper2D}
import helpers.Types.{Coordinate, Grid, GridBuilder}
import noc.Direction._
import noc.NocBuilder.NocInterface
import noc.Position.{EasternEdge, Inside, NorthEasternCorner, NorthWesternCorner, NorthernEdge, SouthEasternCorner, SouthWesternCorner, SouthernEdge, WesternEdge}

object NocBuilder {

  trait NocInterface[P <: Data] {
    val nocIO: Port[P]
  }

  private def neighbors[P <: Data](routers: Seq[Seq[Router[P]]], coordinate: Coordinate): Map[Direction, Option[Port[P]]] = {
    val (x,y) = coordinate.toTuple
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

  def apply[P <: Data](p: NocParameters[P], ports: Seq[Seq[Port[P]]]): Seq[Seq[Router[P]]] = {

    val routers = Position.coordinateGrid(p.size).zip2d(ports).map2d { case ((position, coordinate), port) =>
      val router = Router(coordinate, position)(p)
      println(port)
      router.io.local.get.inbound <> port.outbound
      router.io.local.get.outbound <> port.inbound
      router
    }

    routers.map2d { router =>
      val neighbors = NocBuilder.neighbors(routers, router.coordinate)
      router.io.outbound.filter(_.heading != Local).foreach(port => port.channel <> neighbors(port.heading).get.inbound)
    }

    routers
  }

  def apply[P <: Data](p: NocParameters[P]): Seq[Seq[Router[P]]] = {

    val routers = Position.coordinateGrid(p.size).map2d { case (position, coordinate) =>
      val router = Router(coordinate, position)(p)
      router
    }

    routers.map2d { router =>
      val neighbors = NocBuilder.neighbors(routers, router.coordinate)
      router.io.outbound.filter(_.heading != Local).foreach(port => port.channel <> neighbors(port.heading).get.inbound)
    }

    routers
  }


}

class Adapter[P <: Data](implicit p: NocParameters[P]) extends Module {
  val toNoc = IO(LocalPort())
  val port = IO(LocalPort())

  toNoc.outbound.req := port.inbound.req
  toNoc.outbound.data := port.inbound.data
  port.inbound.ack := toNoc.outbound.ack

  port.outbound.req := toNoc.inbound.req
  port.outbound.data := toNoc.inbound.data
  toNoc.inbound.ack := port.outbound.ack
}


class NOC[P <: Data](implicit p: NocParameters[P]) extends Module {

  val ports = IO(Vec(p.size.m, Vec(p.size.n, LocalPort())))

  val adapters = Seq.fill(p.size.m, p.size.n)(Module(new Adapter))

  NocBuilder(p, adapters.map2d(_.toNoc))

  ports.zip2d(adapters).map2d { case (port, adapter) =>
    port.outbound.req := adapter.port.outbound.req
    port.outbound.data := adapter.port.outbound.data
    adapter.port.outbound.ack := port.outbound.ack

    adapter.port.inbound.req := port.inbound.req
    adapter.port.inbound.data := port.inbound.data
    port.inbound.ack := adapter.port.inbound.ack
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

  implicit val p = NocParameters(3 by 3, () => UInt(8.W))

  val io = IO(HandshakeOut(Packet()))

  val dummies = (Module(new Sender) +: Seq.fill(2)(Module(new Dummy))) +: Seq.fill(2,3)(Module(new Dummy))

  val routers = NocBuilder(p, dummies.map2d(_.nocIO))

  io.req := routers(0)(0).io.local.get.outbound.req
  io.data := routers(0)(0).io.local.get.outbound.data

}

object NocTest extends App { emitVerilog(new NOC()(NocParameters(4 by 4, () => UInt(8.W)))) }
