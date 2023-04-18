package noc

import async.{Empty, Handshake}
import async.blocks.{HandshakeRegister, HandshakeRegisterNext}
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.Types.Coordinate
import noc.Channel.{InboundChannel, OutboundChannel}
import noc.Direction.Local
import noc.RoutingRule.TerminatorRule

class Demux[P <: Data](
                        inDir: Direction,
                        localCoordinate: Coordinate, // coordinate of router
                        routingRule: RoutingRule, // description of routing options
                        registerInput: Boolean = false
                      )(implicit p: NocParameters[P]) extends Module {

  override val desiredName = s"Demux${inDir}_${localCoordinate.x.litValue}_${localCoordinate.y.litValue}"

  val io = IO(new Bundle {
    val in = Flipped(Handshake(Packet()))
    val out = Vec(routingRule.options.length, Handshake(Packet()))
  })

  val in = if(registerInput) HandshakeRegisterNext(io.in, Empty) else io.in

  // create enable signals for output channels based on the incoming header and the local coordinate
  val (outDirections, enableSignals) = routingRule.createLogic(in.data.header, localCoordinate)

  // the input stage clicks if all output channels are stable (req == ack)
  val clickIn = io.out
    .map(h => h.req === h.ack)
    .reduce(_ && _)
    .addSimulationDelay(1)

  // the output stage clicks if the request signal toggles
  val clickOut = (in.req =/= in.ack)
    .addSimulationDelay(1)

  withClockAndReset(clickIn.asClock, reset.asAsyncReset) {
    in.ack := ToggleReg(0.B)
  }

  withClockAndReset(clickOut.asClock, reset.asAsyncReset) {
    enableSignals
      .zip(io.out)
      .foreach { case (takeRoute, port) =>
        port.req := ToggleReg(0.B, takeRoute) // phase register only toggles if its route should be taken
        port.data := in.data
      }
  }
}


object Demux {

  def apply[P <: Data](localCoordinate: Coordinate, position: Position)(inbound: InboundChannel[P])(implicit p: NocParameters[P]): Seq[OutboundChannel[P]] =  {
    RoutingRule(inbound.origin, position) match {
      case TerminatorRule() => Seq(OutboundChannel(Local, inbound.channel))
      case rule =>
        val demux = Module(new Demux(inbound.origin, localCoordinate, rule)).suggestName(s"Demux${inbound.origin}")
        demux.io.in <> inbound.channel
        demux.outDirections.zip(demux.io.out).map { case (dir, channel) => OutboundChannel(dir, channel) }
    }
  }

}

