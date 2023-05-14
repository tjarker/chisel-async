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







