package helpers

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.requireIsHardware


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

  class Coordinate(grid: Grid) extends Bundle {
    val x = UInt(0 until grid.n)
    val y = UInt(0 until grid.m)
  }

  object Coordinate {
    def apply(grid: Grid): Coordinate = new Coordinate(grid)

    def apply(grid: Grid, x: UInt, y: UInt): Coordinate = if (x.isLit && y.isLit) {
      Coordinate(grid).Lit(_.x -> x, _.y -> y)
    } else {
      Coordinate(grid).expand(_.x := x, _.y := y)
    }

    def unapply(c: Coordinate): (UInt, UInt) = c.x -> c.y
  }

}
