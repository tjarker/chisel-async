package noc

import chisel3._
import async._
object Port {
  def apply[P <: Data](direction: Direction, routerPosition: Position)(implicit p: NocParameters[P]): Option[Port[P]] = (routerPosition, direction) match {
    case (NorthernEdge, dir) if dir.isOnNorthSide => None
    case (EasternEdge, dir) if dir.isOnEastSide => None
    case (SouthernEdge, dir) if dir.isOnSouthSide => None
    case (WesternEdge, dir) if dir.isOnWestSide => None
    case (NorthEasternCorner, dir) if dir.isOnNorthSide || dir.isOnEastSide => None
    case (SouthEasternCorner, dir) if dir.isOnEastSide || dir.isOnSouthSide => None
    case (SouthWesternCorner, dir) if dir.isOnSouthSide || dir.isOnWestSide => None
    case (NorthWesternCorner, dir) if dir.isOnWestSide || dir.isOnNorthSide => None
    case _ => Some(new Port(direction))
  }
}

class Port[P <: Data](val dir: Direction)(implicit p: NocParameters[P]) extends Bundle {
  val inbound = Flipped(Handshake(Packet()))
  val outbound = Handshake(Packet())
}