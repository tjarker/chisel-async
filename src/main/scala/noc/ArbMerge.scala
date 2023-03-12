package noc

import chisel3._
import helpers.Hardware.ToggleReg
import helpers.TreeReducer
object ArbMerge {

  // this one takes care of building a tree out of 2:1 arbmerge components
  def apply[P <: Data](channels: Seq[OutboundChannel[P]]): OutboundChannel[P] = channels.reduceTree { (l,r) => ArbMerge(l, r) }

  // you can wrap your 2:1 arbmerge component into this function (connect a and b, then return the output)
  def apply[P <: Data](a: OutboundChannel[P], b: OutboundChannel[P]): OutboundChannel[P] = {
    require(a.heading == b.heading)

    // dummy stuff
    b.channel.ack := ToggleReg(0.B)
    a
  }

}
