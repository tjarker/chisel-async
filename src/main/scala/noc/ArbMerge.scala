package noc

import async.Handshake
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.TreeReducer
import noc.Channel.OutboundChannel


class ArbMerge[P <: Data]()(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Handshake(Packet())))
    val out = Handshake(Packet())
  })

  io.out.req := Mux(ToggleReg(0.B), io.in(0).req, io.in(1).req)
  io.out.data := Mux(ToggleReg(0.B), io.in(0).data, io.in(1).data)
  io.in(0).ack := io.out.ack
  io.in(1).ack := io.out.ack

}

class ArbMergeTree[P <: Data](n: Int)(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Vec(n, Flipped(Handshake(Packet())))
    val out = Handshake(Packet())
  })

  io.out <> io.in.reduceTree { (l, r) =>
    val arbMerge = Module(new ArbMerge())
    arbMerge.io.in(0) <> l
    arbMerge.io.in(1) <> r
    arbMerge.io.out
  }

}

object ArbMerge {

  def apply[P <: Data](channels: Seq[OutboundChannel[P]])(implicit p: NocParameters[P]): OutboundChannel[P] = {
    channels match {
      case Seq(channel) => channel
      case _ =>
        require(channels.tail.forall(_.heading == channels.head.heading))
        val arbMergeTree = Module(new ArbMergeTree(channels.length))
        arbMergeTree.io.in.zip(channels).foreach { case (port, channel) => port <> channel.channel }
        OutboundChannel(channels.head.heading, arbMergeTree.io.out)
    }

  }

}
