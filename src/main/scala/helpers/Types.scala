package helpers

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.requireIsHardware
import helpers.Types.{ConstraintBuilder, Coordinate, GridBuilder}
import noc.{Direction, Position}
import noc.Direction._
import helpers._
import noc.Position._

import scala.util.Random


object Types {

  class Pair[A <: Data, B <: Data](t1: A, t2: B) extends Bundle {
    val _1 = t1
    val _2 = t2
  }

  object Pair {
    def apply[A <: Data, B <: Data](t1: A, t2: B): Pair[A, B] = {
      if(t1.isLit && t2.isLit) new Pair(chiselTypeOf(t1), chiselTypeOf(t2)).Lit(_._1 -> t1, _._2 -> t2)
      else {
        try {
          requireIsHardware(t1)
          requireIsHardware(t2)
          Wire(new Pair(chiselTypeOf(t1), chiselTypeOf(t2))).expand(
            _._1 := t1,
            _._2 := t2
          )
        } catch {
          case e: ExpectedHardwareException => new Pair(t1, t2)
        }
      }
    }

    def unapply[A <: Data, B <: Data](p: Pair[A, B]): Option[(A, B)] = Some(p._1 -> p._2)
  }

  case class Grid(n: Int, m: Int)

  implicit class GridBuilder(n: Int) {
    def by(m: Int): Grid = Grid(n, m)
  }

  class Coordinate(val grid: Grid) extends Bundle {
    val x = UInt(0 until grid.n)
    val y = UInt(0 until grid.m)

    def ==(that: Coordinate): Boolean = x.litValue == that.x.litValue && y.litValue == that.y.litValue
    def is(c: LocationConstraint): Boolean = {
      val dx = x.litValue - c.base.x.litValue
      val dy = y.litValue - c.base.y.litValue
      c.direction match {
        case Local => dx == 0 && dy == 0
        case North => dx == 0 && dy > 0
        case NorthEast => dx > 0 && dy > 0
        case East => dx > 0 && dy == 0
        case SouthEast => dx > 0 && dy < 0
        case South => dx == 0 && dy < 0
        case SouthWest => dx < 0 && dy < 0
        case West => dx < 0 && dy == 0
        case NorthWest => dx < 0 && dy > 0
      }
    }
  }

  case class LocationConstraint(base: Coordinate, direction: Direction)
  implicit class ConstraintBuilder(x: Direction) {
    def of(coord: Coordinate) = LocationConstraint(coord, x)
  }

  object Coordinate {
    def apply(grid: Grid): Coordinate = new Coordinate(grid)

    def apply(grid: Grid, x: UInt, y: UInt): Coordinate = if (x.isLit && y.isLit) {
      Coordinate(grid).Lit(_.x -> x, _.y -> y)
    } else {
      Coordinate(grid).expand(_.x := x, _.y := y)
    }

    def apply(g: Grid, p: Position): Coordinate = {
      val x = p match {
        case Inside | NorthernEdge | SouthernEdge => Random.nextUInt(1 until g.n - 1)
        case EasternEdge | NorthEasternCorner | SouthEasternCorner => (g.n - 1).U
        case WesternEdge | SouthWesternCorner | NorthWesternCorner => 0.U
      }
      val y = p match {
        case Inside | EasternEdge | WesternEdge => Random.nextUInt(1 until g.m - 1)
        case NorthernEdge | NorthEasternCorner | NorthWesternCorner => (g.m - 1).U
        case SouthernEdge | SouthEasternCorner | SouthWesternCorner => 0.U
      }
      Coordinate(g, x, y)
    }

    def apply(c: LocationConstraint): Coordinate = {
      if (c.direction == Local) c.base
      else {
        val lwrX = c.direction match {
          case SouthWest | West | NorthWest => 0
          case North | South => c.base.x.litValue.toInt
          case _ => c.base.x.litValue.toInt + 1
        }
        val uprX = c.direction match {
          case NorthEast | East | SouthEast => c.base.grid.n
          case North | South => c.base.x.litValue.toInt + 1
          case _ => c.base.x.litValue.toInt
        }
        val lwrY = c.direction match {
          case SouthEast | South | SouthWest => 0
          case West | East => c.base.y.litValue.toInt
          case _ => c.base.y.litValue.toInt + 1
        }
        val uprY = c.direction match {
          case NorthWest | North | NorthEast => c.base.grid.m
          case West | East => c.base.y.litValue.toInt + 1
          case _ => c.base.y.litValue.toInt
        }
        val x = if(lwrX == uprX) lwrX.U else Random.nextUInt(lwrX until uprX)
        val y = if(lwrY == uprY) lwrY.U else Random.nextUInt(lwrY until uprY)
        Coordinate(c.base.grid, x, y)
      }
    }

    def unapply(c: Coordinate): (UInt, UInt) = c.x -> c.y
  }

}