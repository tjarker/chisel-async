package noc

import async.Empty
import chisel3._
import helpers.EmitVerilog
import helpers.Types.{Coordinate, GridBuilder}
import noc.Direction.{East, Local, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}
import noc.Router.RouterIO
import Channel._

class Router[P <: Data](coordinate: Coordinate, position: Position)(implicit p: NocParameters[P]) extends Module {

  val io = IO(RouterIO(position))

  println(io.inbound.map(_.origin))

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

    private def all: Seq[Port[P]] = Seq(local, north, northeast, east, southeast, south, southwest, west, northwest).collect {
      case Some(port) => port
    }
    def inbound: Seq[InboundChannel[P]] = all.map { port => InboundChannel(port.dir, port.inbound) }
    def outbound: Seq[OutboundChannel[P]] = all.map { port => OutboundChannel(port.dir, port.outbound) }
  }

  def apply[P <: Data](coordinate: Coordinate, position: Position)(implicit p: NocParameters[P]): Router[P] = Module(new Router(coordinate, position))
}

object RouterTop extends EmitVerilog({
  val size = 10 by 10
  new Router(Coordinate(size, 5.U, 4.U), NorthernEdge)(NocParameters(size, () => UInt(8.W)))
})