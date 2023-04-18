package noc

import async.{Empty, Handshake}
import chisel3._
import helpers.EmitVerilog
import helpers.Types.{Coordinate, GridBuilder}
import noc.Direction.{East, Local, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}
import noc.Router.RouterIO
import Channel._
import noc.Position.NorthernEdge

class Router[P <: Data](val coordinate: Coordinate, val position: Position)(implicit p: NocParameters[P]) extends Module {
  override val desiredName = s"Router@$position$coordinate"

  val io = IO(RouterIO(position))

  println(s"Router@$position$coordinate")

  io.inbound
    .map(_.addHandshakeRegister(Empty)) // add a stage of handshake registers to inbound channels
    .flatMap(Demux(coordinate, position)(_)) // demux inbound channels to possible outbound channel connections
    .groupByDirection // group outbound channels of same direction
    .map(ArbMerge(_)) // arbitrate between outbound channels of same direction
    .connect(io.outbound)

}

object Router {

  object RouterIO {
    def apply[P <: Data](position: Position)(implicit p: NocParameters[P]): RouterIO[P] = new RouterIO(position)
  }

  class RouterIO[P <: Data](position: Position)(implicit p: NocParameters[P]) extends Bundle {
    val local = Port(Local, position)
    val north = Port(North, position)
    val northeast = Port(NorthEast, position)
    val east = Port(East, position)
    val southeast = Port(SouthEast, position)
    val south = Port(South, position)
    val southwest = Port(SouthWest, position)
    val west = Port(West, position)
    val northwest = Port(NorthWest, position)

    def all: Seq[Port[P]] = Seq(local, north, northeast, east, southeast, south, southwest, west, northwest).collect {
      case Some(port) => port
    }
    def inbound: Seq[InboundChannel[P]] = all.map { port => InboundChannel(port.dir, port.inbound) }
    def inboundMap: Map[Direction, Handshake[Packet[P]]] = all.map { port => port.dir -> port.inbound }.toMap
    def outbound: Seq[OutboundChannel[P]] = all.map { port => OutboundChannel(port.dir, port.outbound) }
    def outboundMap: Map[Direction, Handshake[Packet[P]]] = all.map { port => port.dir -> port.outbound }.toMap
  }

  def apply[P <: Data](coordinate: Coordinate, position: Position)(implicit p: NocParameters[P]): Router[P] = Module(new Router(coordinate, position)).suggestName(s"Router@$position$coordinate")
}

object RouterTop extends EmitVerilog({
  val size = 10 by 10
  new Router(Coordinate(size, 5.U, 4.U), NorthernEdge)(NocParameters(size, () => UInt(8.W)))
})