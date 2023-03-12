package noc

import async.Empty
import chisel3._
import helpers.EmitVerilog
import helpers.Types.{Coordinate, GridBuilder}
import noc.Direction.{East, Local, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}
import noc.Router.RouterIO

class Router[P <: Data](coordinate: Coordinate)(implicit p: NocParameters[P]) extends Module {

  val io = IO(RouterIO())

  io.outbound <> io.inbound
    .map(_.addHandshakeRegister(Empty)) // add a stage of handshake registers to inbound channels
    .flatMap(Demux(coordinate)(_)) // demux inbound channels to possible outbound channel connections
    .groupByDirection // group outbound channels of same direction
    .map(ArbMerge(_)) // arbitrate between outbound channels of same direction

}

object Router {

  object RouterIO {
    def apply[P <: Data]()(implicit p: NocParameters[P]): RouterIO[P] = new RouterIO
  }

  class RouterIO[P <: Data](implicit p: NocParameters[P]) extends Bundle {
    val local = Port(Local)
    val north = Port(North)
    val northeast = Port(NorthEast)
    val east = Port(East)
    val southeast = Port(SouthEast)
    val south = Port(South)
    val southwest = Port(SouthWest)
    val west = Port(West)
    val northwest = Port(NorthWest)

    private def all: Seq[Port[P]] = Seq(local, north, northeast, east, southeast, south, southwest, west, northwest)
    def inbound: Seq[InboundChannel[P]] = all.map { port => InboundChannel(port.dir, port.inbound) }
    def outbound: Seq[OutboundChannel[P]] = all.map { port => OutboundChannel(port.dir, port.outbound) }
  }

  def apply[P <: Data](coordinate: Coordinate)(implicit p: NocParameters[P]): Router[P] = Module(new Router(coordinate))
}

object RouterTop extends EmitVerilog({
  val size = 10 by 10
  new Router(Coordinate(size, 5.U, 4.U))(NocParameters(size, () => UInt(8.W)))
})