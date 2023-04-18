package noc

import async.Handshake
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

class Dummy()(implicit p: NocParameters[UInt]) extends Module with NocInterface[UInt] {
  override val nocIO: Port[UInt] = IO(LocalPort())


  val sink = Sink(nocIO.inbound)
  val source = Source(Packet().Lit(_.payload -> 0xFF.U))
  source <> nocIO.outbound

}

class NocTest extends Module {

  implicit val p = NocParameters(2 by 2, () => UInt(8.W))

  val io = IO(Handshake(Packet()))

  val dummies = Seq.fill(2,2)(Module(new Dummy))

  val routers = NocBuilder(p, dummies)

  io.req := routers(0)(0).io.local.get.outbound.req
  io.data := routers(0)(0).io.local.get.outbound.data

}

object NocTest extends App { emitVerilog(new NocTest) }
