package noc

import scala.util.matching.Regex.Match


abstract class Direction {
  import Direction._
  def isDiagonal = this match {
    case NorthEast | SouthEast | SouthWest | NorthWest => true
    case _ => false
  }
  def isVertical = this match {
    case North | South => true
    case _ => false
  }
  def isHorizontal = this match {
    case East | West => true
    case _ => false
  }
  def isOnNorthSide: Boolean = this match {
    case NorthWest | North | NorthEast => true
    case _ => false
  }
  def isOnEastSide: Boolean = this match {
    case NorthEast | East | SouthEast => true
    case _ => false
  }
  def isOnSouthSide: Boolean = this match {
    case SouthEast | South | SouthWest => true
    case _ => false
  }
  def isOnWestSide: Boolean = this match {
    case NorthWest | West | SouthWest => true
    case _ => false
  }
  def horizontalNeighbor = if(this.clockwiseNeighbor.isHorizontal) this.clockwiseNeighbor else this.counterClockwiseNeighbor
  def verticalNeighbor = if(this.clockwiseNeighbor.isVertical) this.clockwiseNeighbor else this.counterClockwiseNeighbor
  def opposite: Direction = this match {
    case Local => Local
    case North => South
    case NorthEast => SouthWest
    case East => West
    case SouthEast => NorthWest
    case South => North
    case SouthWest => NorthEast
    case West => East
    case NorthWest => SouthEast
  }

  def clockwiseNeighbor: Direction = this match {
    case Local => Local
    case North => NorthEast
    case NorthEast => East
    case East => SouthEast
    case SouthEast => South
    case South => SouthWest
    case SouthWest => West
    case West => NorthWest
    case NorthWest => North
  }

  def counterClockwiseNeighbor: Direction = this match {
    case Local => Local
    case North => NorthWest
    case NorthEast => North
    case East => NorthEast
    case SouthEast => East
    case South => SouthEast
    case SouthWest => South
    case West => SouthWest
    case NorthWest => West
  }
}

object Direction {
  case object Local extends Direction

  case object North extends Direction

  case object NorthEast extends Direction

  case object East extends Direction

  case object SouthEast extends Direction

  case object South extends Direction

  case object SouthWest extends Direction

  case object West extends Direction

  case object NorthWest extends Direction

  def all: Seq[Direction] = Seq(Local, North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
}