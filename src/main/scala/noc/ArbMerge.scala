package noc

import async.Handshake
import chisel3._
object ArbMerge {

  def apply[P <: Data](channels: Seq[Handshake[Packet[P]]]): Handshake[Packet[P]] = ???

}
