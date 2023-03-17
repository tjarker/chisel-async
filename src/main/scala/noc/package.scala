import async.blocks.HandshakeRegisterNext
import async.{Handshake, HandshakeInitializer}
import chisel3._
import helpers.Types.{Coordinate, Grid}
import noc.Direction._
import noc.Router._

package object noc {

  case class NocParameters[P <: Data](size: Grid, payloadGen: () => P)



  object Packet {
    def apply[P <: Data]()(implicit p: NocParameters[P]): Packet[P] = new Packet
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





}
