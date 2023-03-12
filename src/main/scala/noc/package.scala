import async.blocks.HandshakeRegisterNext
import async.{Handshake, HandshakeInitializer}
import chisel3._
import helpers.Types.{Coordinate, Grid}
import noc.Direction._

package object noc {

  case class NocParameters[P <: Data](size: Grid, payloadGen: () => P)

  object Port {
    def apply[P <: Data](direction: Direction)(implicit p: NocParameters[P]): Port[P] = new Port(direction)
  }
  class Port[P <: Data](val dir: Direction)(implicit p: NocParameters[P]) extends Bundle {
    val inbound = Flipped(Handshake(Packet()))
    val outbound = Handshake(Packet())
  }

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



  case class InboundChannel[P <: Data](origin: Direction, channel: Handshake[Packet[P]]) {
    def addHandshakeRegister(init: HandshakeInitializer[Packet[P]]): InboundChannel[P] =
      InboundChannel(origin, HandshakeRegisterNext(channel, init))
  }
  case class OutboundChannel[P <: Data](heading: Direction, channel: Handshake[Packet[P]]) {
    def <>(that: OutboundChannel[P]) = {
      require(heading == that.heading)
      channel <> that.channel
    }
  }
  implicit class OutboundChannelSeqExtension[P <: Data](x: Seq[OutboundChannel[P]]) {
    def groupByDirection: Seq[Seq[OutboundChannel[P]]] = Direction.all.map(d => x.filter(_.heading == d))
    def <>(that: Seq[OutboundChannel[P]]) = {
      require(x.length == that.length)
      x.zip(that).foreach { case (l,r) => l <> r }
    }
  }

}
