package noc

import chisel3._
import async._

object Port {
  def apply[P <: Data](direction: Direction, routerPosition: Position)(implicit p: NocParameters[P]): Option[Port[P]] =
    if(routerPosition.hasPort(direction)) Some(new Port(direction)) else None
}

class Port[P <: Data](val dir: Direction)(implicit p: NocParameters[P]) extends Bundle {
  val inbound = Flipped(Handshake(Packet()))
  val outbound = Handshake(Packet())
}