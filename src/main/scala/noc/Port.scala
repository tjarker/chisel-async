package noc

import chisel3._
import async._
import noc.Direction.Local

object Port {
  def apply[P <: Data](direction: Direction, routerPosition: Position)(implicit p: NocParameters[P]): Option[Port[P]] =
    if(routerPosition.hasPort(direction)) Some(new Port(direction)) else None
}

class Port[P <: Data](val dir: Direction)(implicit p: NocParameters[P]) extends Bundle {
  val inbound = HandshakeIn(Packet())
  val outbound = HandshakeOut(Packet())

  def <>(that: Port[P]): Unit = {
    inbound <> that.outbound
    outbound <> that.inbound
  }
}

object LocalPort {
  def apply[P <: Data]()(implicit p: NocParameters[P]): Port[P] = new Port(Local)
}