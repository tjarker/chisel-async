package noc

import async.Helper._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.internal.firrtl.Width
import noc.Helper.{Grid, GridBuilder}

case class NocParameters(size: Grid)

class Packet[P <: Data](payloadGen: P, p: NocParameters) extends Bundle {
  val header = new Header(p)
  val payload = payloadGen
}



class Header(p: NocParameters) extends Bundle {
  val destination = Coordinate(p.size)
  val sign = new Bundle {
    val dx = Bool()
    val dy = Bool()
  }
}

class Coordinate(grid: Grid) extends Bundle {
  val x = UInt(0 until grid.n)
  val y = UInt(0 until grid.m)
}
object Coordinate {
  def apply(grid: Grid): Coordinate = new Coordinate(grid)
  def apply(grid: Grid, x: UInt, y: UInt): Coordinate = if(x.isLit && y.isLit) {
    Coordinate(grid).Lit(_.x -> x, _.y -> y)
  } else {
    Coordinate(grid).expand(_.x := x, _.y := y)
  }
  def unapply(c: Coordinate): (UInt, UInt) = c.x -> c.y
}