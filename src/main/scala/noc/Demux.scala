package noc

import async.Handshake
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.Types.Coordinate

class Demux[P <: Data](
                        localCoordinate: Coordinate, // coordinate of router
                        routingRule: RoutingRule // description of routing options
                      )(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(Packet()))
    val out = Vec(routingRule.options, Handshake(Packet()))
  })

  // create enable signals for output channels based on the incoming header and the local coordinate
  val (outDirections, enableSignals) = routingRule.createLogic(io.in.data.header, localCoordinate)

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

  def apply[P <: Data](localCoordinate: Coordinate)(inbound: InboundChannel[P])(implicit p: NocParameters[P]): Seq[OutboundChannel[P]] =  {
    val demux = Module(new Demux(localCoordinate, RoutingRule(inbound.origin)))
    demux.io.in <> inbound.channel
    demux.outDirections.zip(demux.io.out).map { case (dir, channel) => OutboundChannel(dir, channel) }
  }

}

