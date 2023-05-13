package noc

import chisel3._
import helpers.Types.{Coordinate, Grid, GridBuilder}


abstract class Position {
  import noc.Position._
  def isCorner = this match {
    case NorthEasternCorner | SouthEasternCorner | SouthWesternCorner | NorthWesternCorner => true
    case _ => false
  }
  def isHorizontalEdge = this match {
    case NorthernEdge | SouthernEdge => true
    case _ => false
  }
  def isVerticalEdge = this match {
    case EasternEdge | WesternEdge => true
    case _ => false
  }

  def hasPort(dir: Direction): Boolean = (this, dir) match {
    case (NorthernEdge, dir) if dir.isOnNorthSide => false
    case (EasternEdge, dir) if dir.isOnEastSide => false
    case (SouthernEdge, dir) if dir.isOnSouthSide => false
    case (WesternEdge, dir) if dir.isOnWestSide => false
    case (NorthEasternCorner, dir) if dir.isOnNorthSide || dir.isOnEastSide => false
    case (SouthEasternCorner, dir) if dir.isOnEastSide || dir.isOnSouthSide => false
    case (SouthWesternCorner, dir) if dir.isOnSouthSide || dir.isOnWestSide => false
    case (NorthWesternCorner, dir) if dir.isOnWestSide || dir.isOnNorthSide => false
    case _ => true
  }

}

object Position {

  case object Inside extends Position

  case object NorthernEdge extends Position

  case object EasternEdge extends Position

  case object SouthernEdge extends Position

  case object WesternEdge extends Position

  case object NorthEasternCorner extends Position

  case object SouthEasternCorner extends Position

  case object SouthWesternCorner extends Position

  case object NorthWesternCorner extends Position
  def all: Seq[Position] = Seq(Inside, NorthernEdge, EasternEdge, SouthernEdge, WesternEdge, NorthEasternCorner, SouthEasternCorner, SouthWesternCorner, NorthWesternCorner)

  def grid(size: Grid): Seq[Seq[Position]] = {
    val south = SouthWesternCorner +: Seq.fill(size.n - 2)(SouthernEdge) :+ SouthEasternCorner
    val north = NorthWesternCorner +: Seq.fill(size.n - 2)(NorthernEdge) :+ NorthEasternCorner
    val middle = WesternEdge +: Seq.fill(size.n - 2)(Inside) :+ EasternEdge

    south +: Seq.fill(size.m - 2)(middle) :+ north
  }
  def coordinateGrid(size: Grid): Seq[Seq[(Position, Coordinate)]] = {
    grid(size).map(_.zipWithIndex).zipWithIndex.map { case (rows, y) =>
      rows.map { case (pos, x) => pos -> Coordinate(size, x.U, y.U) }
    }
  }
}