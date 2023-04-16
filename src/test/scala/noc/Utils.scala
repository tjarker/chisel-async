package noc

import chisel3._
import chiseltest._
import helpers.Types.Coordinate
import noc.Direction._

object Utils {

  def testRoute(inDir: Direction, goWest: Boolean, goSouth: Boolean, xMatch: Boolean, yMatch: Boolean): Direction = {
    if (inDir == Local) {
      (xMatch, yMatch, goWest, goSouth) match {
        case (true, true, _, _) => throw new Exception("Local to Local")
        case (true, false, _, false) => North
        case (true, false, _, true) => South
        case (false, true, false, _) => East
        case (false, true, true, _) => West
        case (false, false, false, false) => NorthEast
        case (false, false, false, true) => SouthEast
        case (false, false, true, true) => SouthWest
        case (false, false, true, false) => NorthWest
      }
    } else (xMatch, yMatch, goWest, goSouth) match {
      case (true, true, _, _) => Local
      case (true, false, _, _) => if (inDir.isDiagonal) inDir.opposite.horizontalNeighbor else inDir.opposite
      case (false, true, _, _) => if (inDir.isDiagonal) inDir.opposite.verticalNeighbor else inDir.opposite
      case (false, false, _, _) => inDir.opposite
    }
  }

  def validate(localCoordinate: Coordinate, inDir: Direction, outDir: Direction, header: Header) = {
    println(header.peek())
    val xMatch = localCoordinate.x.litValue == header.destination.x.peek().litValue
    val yMatch = localCoordinate.y.litValue == header.destination.y.peek().litValue
    val hit = xMatch && yMatch
    val goSouth = header.sign.dy.peekBoolean()
    val goWest = header.sign.dx.peekBoolean()
    (inDir, outDir) match {
      case North -> South => !yMatch
      case North -> Local => yMatch

      case South -> North => !yMatch
      case South -> Local => yMatch

      case East -> West => !xMatch
      case East -> Local => yMatch

      case West -> East => !xMatch
      case West -> Local => xMatch


      case NorthEast -> South => xMatch
      case NorthEast -> SouthWest => !hit
      case NorthEast -> West => yMatch
      case NorthEast -> Local => hit

      case SouthEast -> West => yMatch
      case SouthEast -> NorthWest => !hit
      case SouthEast -> North => xMatch
      case SouthEast -> Local => hit

      case SouthWest -> North => xMatch
      case SouthWest -> NorthEast => !hit
      case SouthWest -> East => yMatch
      case SouthWest -> Local => hit

      case NorthWest -> East => yMatch
      case NorthWest -> SouthEast => !hit
      case NorthWest -> South => xMatch
      case NorthWest -> Local => hit

      case Local -> North => xMatch && !yMatch && !goSouth
      case Local -> NorthEast => !hit && !goSouth && !goWest
      case Local -> East => !xMatch && yMatch && !goWest
      case Local -> SouthEast => !hit && goSouth && !goWest
      case Local -> South => xMatch && !yMatch && goSouth
      case Local -> SouthWest => !hit && goSouth && goWest
      case Local -> West => !xMatch && yMatch && goWest
      case Local -> NorthWest => !hit && !goSouth && goWest

      case _ => false
    }
  }

}
