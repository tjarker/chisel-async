package noc

import chisel3._
import async._
import async.blocks.HandshakeRegisterNext
object Channel {

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
    def groupByDirection: Seq[Seq[OutboundChannel[P]]] = Direction.all.map(d => x.filter(_.heading == d)).filter(_.nonEmpty)

    def connect(sink: Seq[OutboundChannel[P]]) = {
      require(x.length == sink.length)
      x.zip(sink).foreach { case (l, r) => l <> r }
    }
  }

}
