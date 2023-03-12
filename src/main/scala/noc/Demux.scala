package noc


import async.Handshake
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.Types.Coordinate




class Demux[P <: Data](
                        n: Int, // number of output channels
                        localCoordinate: Coordinate, // coordinate of router
                        routingTableGen: (Header, Coordinate) => Seq[(Direction, Bool)] // boolean functions for routing signals
                      )(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(Packet()))
    val out = Vec(n, Handshake(Packet()))
  })

  // create enable signals for output channels based on the incoming header and the local coordinate
  val (outDirections, enableSignals) = routingTableGen(io.in.data.header, localCoordinate).unzip

  // the input stage clicks if all output channels are stable (req == ack)
  val clickIn = io.out
    .map(h => h.req === h.ack)
    .reduce(_ && _)
    .addSimulationDelay(1)

  // the output stage clicks if the request signal toggles
  val clickOut = (io.in.req =/= io.in.ack)
    .addSimulationDelay(1)

  withClockAndReset(clickIn.asClock, reset.asAsyncReset) {
    io.in.ack := ToggleReg(0.B)
  }

  withClockAndReset(clickOut.asClock, reset.asAsyncReset) {
    enableSignals
      .zip(io.out)
      .foreach { case (takeRoute, port) =>
        port.req := ToggleReg(0.B, takeRoute) // phase register only toggles if its route should be taken
        port.data := io.in.data
      }
  }
}


object Demux {

  def apply[P <: Data](localCoordinate: Coordinate)(inbound: InboundChannel[P])(implicit p: NocParameters[P]): Seq[OutboundChannel[P]] = {
    val route = RoutingRule(inbound.origin)
    apply(route.options, localCoordinate, route.routingTable, inbound.channel)
  }

  def apply[P <: Data](n: Int, localCoordinate: Coordinate, routingTable: (Header, Coordinate) => Seq[(Direction, Bool)], channel: Handshake[Packet[P]])(implicit p: NocParameters[P]): Seq[OutboundChannel[P]] = {
    val demux = Module(new Demux(n, localCoordinate, routingTable))
    demux.io.in <> channel
    demux.outDirections.zip(demux.io.out).map { case (dir, channel) => OutboundChannel(dir, channel) }
  }


}

