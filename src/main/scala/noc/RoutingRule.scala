package noc

import chisel3._
import helpers.Types.Coordinate
import noc.Direction._

// a routing rule describes the paths which can be taken through the router given the inbound direction
sealed trait RoutingRule {
  // the routing table generates the logic for a one-hot vector where each bit is associated with a direction
  def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool])
  def options: Int // the number of possible paths through the router
}

object RoutingRule {

  // a route is created depending on the input direction
  def apply(in: Direction): RoutingRule =
    if(in == Local) {
      LocalRule()
    } else if (in.isDiagonal) {
      DiagonalRule(in.opposite, in.opposite.horizontalNeighbor, in.opposite.verticalNeighbor)
    } else if(in.isHorizontal) {
      HorizontalRule(in.opposite)
    } else {
      VerticalRule(in.opposite)
    }

  // packets which arrive horizontally can only stay or proceed horizontally
  case class HorizontalRule(straightOption: Direction) extends RoutingRule {
    def options = 2
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      Seq(
        Local -> xMatch,
        straightOption -> !xMatch
      ).unzip
    }
  }

  // packets which arrive vertically can only stay or proceed vertically
  case class VerticalRule(straightOption: Direction) extends RoutingRule {
    def options = 2
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val yMatch = header.destination.y === localCoord.y
      Seq(
        Local -> yMatch,
        straightOption -> !yMatch
      ).unzip
    }
  }

  // diagonal packets might stay, continue vertically/horizontally in case of a partial coordinate match
  // or proceed diagonally in case no coordinate matches
  case class DiagonalRule(diagonal: Direction, horizontal: Direction, vertical: Direction) extends RoutingRule {
    def options = 4
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      Seq(
        Local -> (xMatch && yMatch),
        diagonal -> (!xMatch && !yMatch),
        horizontal -> (!xMatch && yMatch),
        vertical -> (xMatch && !yMatch)
      ).unzip
    }
  }

  // local packets may take any of the 8 directions depending on the destination coordinate and the
  // signs of the vector from the current coordinate to the destination coordinate
  case class LocalRule() extends RoutingRule {
    def options = 8
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      val sdx = header.sign.dx
      val sdy = header.sign.dy
      val takeDiagonal = !xMatch && !yMatch
      Seq(
        North -> (xMatch && !sdx),
        East -> (yMatch && !sdy),
        South -> (xMatch && sdx),
        West -> (yMatch && sdy),
        NorthEast -> (takeDiagonal && !sdx && !sdy),
        SouthEast -> (takeDiagonal && !sdx && sdy),
        SouthWest -> (takeDiagonal && sdx && sdy),
        NorthWest -> (takeDiagonal && sdx && !sdy)
      ).unzip
    }
  }
}




