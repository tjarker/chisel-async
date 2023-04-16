package noc

import chisel3._
import helpers.Types.Coordinate
import noc.Direction._
import noc.Position._

// a routing rule describes the paths which can be taken through the router given the inbound direction
sealed trait RoutingRule {
  // the routing table generates the logic for a one-hot vector where each bit is associated with a direction
  def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool])
  def options: Seq[Direction]
}

object RoutingRule {

  // a route is created depending on the input direction
  def apply(in: Direction, position: Position): RoutingRule = (position, in) match {
    case (Inside, Local) => LocalRule()
    case (NorthEasternCorner, Local) => LocalCornerRule(SouthWest)
    case (SouthEasternCorner, Local) => LocalCornerRule(NorthWest)
    case (SouthWesternCorner, Local) => LocalCornerRule(NorthEast)
    case (NorthWesternCorner, Local) => LocalCornerRule(SouthEast)
    case (NorthernEdge, Local) => LocalTopEdgeRule()
    case (EasternEdge, Local) => LocalRightEdgeRule()
    case (SouthernEdge, Local) => LocalBottomEdgeRule()
    case (WesternEdge, Local) => LocalLeftEdgeRule()
    case (Inside, dir) if dir.isDiagonal => DiagonalRule(dir)
    case (pos, _) if pos.isCorner => TerminatorRule()


    case (pos, dir) if pos.isVerticalEdge && dir.isHorizontal => TerminatorRule()
    case (pos, dir) if pos.isHorizontalEdge && dir.isVertical => TerminatorRule()
    case (pos, dir) if pos.isVerticalEdge && dir.isDiagonal => DiagonalOnVerticalEdgeRule(dir)
    case (pos, dir) if pos.isHorizontalEdge && dir.isDiagonal => DiagonalOnHorizontalEdgeRule(dir)
    case (_, dir) if dir.isHorizontal => HorizontalRule(dir)
    case (_, dir) if dir.isVertical => VerticalRule(dir)
  }


  // packets which arrive horizontally can only stay or proceed horizontally
  case class HorizontalRule(incoming: Direction) extends RoutingRule {
    def options = Seq(Local, incoming.opposite)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      Seq(
        Local -> xMatch,
        incoming.opposite -> !xMatch
      ).unzip
    }
  }

  // packets which arrive vertically can only stay or proceed vertically
  case class VerticalRule(incoming: Direction) extends RoutingRule {
    def options = Seq(Local, incoming.opposite)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val yMatch = header.destination.y === localCoord.y
      Seq(
        Local -> yMatch,
        incoming.opposite -> !yMatch
      ).unzip
    }
  }

  // diagonal packets might stay, continue vertically/horizontally in case of a partial coordinate match
  // or proceed diagonally in case no coordinate matches
  case class DiagonalRule(incoming: Direction) extends RoutingRule {
    def options = Seq(Local, incoming.opposite, incoming.opposite.horizontalNeighbor, incoming.opposite.verticalNeighbor)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      Seq(
        Local -> (xMatch && yMatch),
        incoming.opposite -> (!xMatch && !yMatch),
        incoming.opposite.horizontalNeighbor -> (!xMatch && yMatch),
        incoming.opposite.verticalNeighbor -> (xMatch && !yMatch)
      ).unzip
    }
  }

  // local packets may take any of the 8 directions depending on the destination coordinate and the
  // signs of the vector from the current coordinate to the destination coordinate
  case class LocalRule() extends RoutingRule {
    def options = Seq(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      val goWest = header.sign.dx
      val goSouth = header.sign.dy
      val takeDiagonal = !xMatch && !yMatch
      Seq(
        North -> (xMatch && !goSouth),
        East -> (yMatch && !goWest),
        South -> (xMatch && goSouth),
        West -> (yMatch && goWest),
        NorthEast -> (takeDiagonal && !goWest && !goSouth),
        SouthEast -> (takeDiagonal && !goWest && goSouth),
        SouthWest -> (takeDiagonal && goWest && goSouth),
        NorthWest -> (takeDiagonal && goWest && !goSouth)
      ).unzip
    }
  }

  case class LocalCornerRule(inWardDiagonal: Direction) extends RoutingRule {
    def options = Seq(inWardDiagonal, inWardDiagonal.verticalNeighbor, inWardDiagonal.horizontalNeighbor)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      Seq(
        inWardDiagonal -> (!xMatch && !yMatch),
        inWardDiagonal.verticalNeighbor -> xMatch,
        inWardDiagonal.horizontalNeighbor -> yMatch
      ).unzip
    }
  }

  case class LocalTopEdgeRule() extends RoutingRule {
    def options = Seq(East, South, West, SouthEast, SouthWest)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      val goWest = header.sign.dx
      val takeDiagonal = !xMatch && !yMatch
      Seq(
        East -> (yMatch && !goWest),
        South -> (xMatch),
        West -> (yMatch && goWest),
        SouthEast -> (takeDiagonal && !goWest),
        SouthWest -> (takeDiagonal && goWest),
      ).unzip
    }
  }

  case class LocalBottomEdgeRule() extends RoutingRule {
    def options = Seq(North, East, West, NorthEast, NorthWest)

    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      val goWest = header.sign.dx
      val takeDiagonal = !xMatch && !yMatch
      Seq(
        North -> (xMatch),
        East -> (yMatch && !goWest),
        West -> (yMatch && goWest),
        NorthEast -> (takeDiagonal && !goWest),
        NorthWest -> (takeDiagonal && goWest)
      ).unzip
    }
  }

  case class LocalLeftEdgeRule() extends RoutingRule {
    def options = Seq(North, East, South, NorthEast, SouthEast)

    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      val goSouth = header.sign.dy
      val takeDiagonal = !xMatch && !yMatch
      Seq(
        North -> (xMatch && !goSouth),
        East -> (yMatch),
        South -> (xMatch && goSouth),
        NorthEast -> (takeDiagonal && !goSouth),
        SouthEast -> (takeDiagonal && goSouth),
      ).unzip
    }
  }

  case class LocalRightEdgeRule() extends RoutingRule {
    def options = Seq(North, South, West, SouthWest, NorthWest)

    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      val yMatch = header.destination.y === localCoord.y
      val goSouth = header.sign.dy
      val takeDiagonal = !xMatch && !yMatch
      Seq(
        North -> (xMatch && !goSouth),
        South -> (xMatch && goSouth),
        West -> (yMatch),
        SouthWest -> (takeDiagonal && goSouth),
        NorthWest -> (takeDiagonal && !goSouth)
      ).unzip
    }
  }


  case class TerminatorRule() extends RoutingRule {
    def options = Seq(Local)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = Seq(Local) -> Seq(1.B)
  }

  case class DiagonalOnVerticalEdgeRule(incoming: Direction) extends RoutingRule {
    def options = Seq(Local, incoming.opposite.verticalNeighbor)
    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val yMatch = header.destination.y === localCoord.y
      Seq(
        Local -> yMatch,
        incoming.opposite.verticalNeighbor -> !yMatch
      ).unzip
    }
  }

  case class DiagonalOnHorizontalEdgeRule(incoming: Direction) extends RoutingRule {
    def options = Seq(Local, incoming.opposite.horizontalNeighbor)

    def createLogic(header: Header, localCoord: Coordinate): (Seq[Direction], Seq[Bool]) = {
      val xMatch = header.destination.x === localCoord.x
      Seq(
        Local -> xMatch,
        incoming.opposite.horizontalNeighbor -> !xMatch
      ).unzip
    }
  }
}




