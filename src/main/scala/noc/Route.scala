package noc

import chisel3._
import noc.Direction.Local

sealed trait Route {
  def routingTable(header: Header, localCoord: Coordinate): Seq[(Direction, Bool)]
  def options: Int
}

object Route {

  def apply(in: Direction): Route = if (in.isDiagonal) {
    DiagonalEntry(in.opposite, in.opposite.horizontalNeighbor, in.opposite.verticalNeighbor)
  } else if(in.isHorizontal) {
    HorizontalStraightEntry(in.opposite)
  } else {
    VerticalStraightEntry(in.opposite)
  }

  case class HorizontalStraightEntry(straightOption: Direction) extends Route {
    def options = 2
    def routingTable(header: Header, localCoord: Coordinate): Seq[(Direction, Bool)] = {
      val xMatch = header.destination.x === localCoord.x
      Seq(
        Local -> xMatch,
        straightOption -> !xMatch
      )
    }
  }

  case class VerticalStraightEntry(straightOption: Direction) extends Route {
    def options = 2
    def routingTable(header: Header, localCoord: Coordinate): Seq[(Direction, Bool)] = {
      val yMatch = header.destination.y === localCoord.y
      Seq(
        Local -> yMatch,
        straightOption -> !yMatch
      )
    }
  }

  case class DiagonalEntry(diagonal: Direction, horizontal: Direction, vertical: Direction) extends Route {
    def options = 4
    def routingTable(header: Header, localCoord: Coordinate): Seq[(Direction, Bool)] = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      Seq(
        Local -> (xMatch && yMatch),
        diagonal -> (!xMatch && !yMatch),
        horizontal -> (!xMatch && yMatch),
        vertical -> (xMatch && !yMatch)
      )
    }
  }
}




