
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.log2Ceil
import helpers.Types.{Coordinate, Grid}


package object noc {

  case class NocParameters[P <: Data](size: Grid, payloadGen: () => P) {
    val xw = log2Ceil(size.n).W
    val yw = log2Ceil(size.m).W
  }



  object Packet {
    def apply[P <: Data]()(implicit p: NocParameters[P]): Packet[P] = new Packet

    def apply[P <: Data](journey: (Coordinate, Coordinate), data: P)(implicit p: NocParameters[P]): Packet[P] = (new Packet).Lit(
      _.payload -> data,
      _.header -> Header(journey)
    )
  }
  class Packet[P <: Data](implicit p: NocParameters[P]) extends Bundle {
    val header = new Header
    val payload = p.payloadGen()
  }

  class Header(implicit p: NocParameters[_]) extends Bundle {
    val destination = Coordinate(p.size)
    val sign = new Bundle {
      val dx = Bool()
      val dy = Bool()
    }
  }

  object Header {
    def apply(journey: (Coordinate, Coordinate))(implicit p: NocParameters[_]): Header =
      (new Header()).Lit(
        _.destination -> journey._2,
        _.sign.dx -> (journey._1.x.litValue > journey._2.x.litValue).B,
        _.sign.dy -> (journey._1.y.litValue > journey._2.y.litValue).B
      )
  }




}
