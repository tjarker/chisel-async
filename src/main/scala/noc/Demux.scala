package noc

import async.Empty
import async.Handshake
import async.Helper.{SeqToChiselVec, ToggleReg}
import async.blocks.{HandshakeRegister, HandshakeRegisterNext}
import async.blocks.SimulationDelay.SimulationDelayer
import chisel3._
import noc.Direction.{North, NorthEast}
import noc.Helper.GridBuilder







class Demux[P <: Data](n: Int, pgen: P, p: NocParameters, localCoordinate: Coordinate, routingTableGen: (Header, Coordinate) => Seq[(Direction, Bool)]) extends Module {


  val io = IO(new Bundle {
    val in = Flipped(Handshake(new Packet(pgen, p)))
    val out = Vec(n, Handshake(new Packet(pgen, p)))
  })

  val routingTable = routingTableGen(io.in.data.header, localCoordinate)

  val outDirections = routingTable.map(_._1)

  val clickIn = io.out.map(h => h.req === h.ack).reduce(_ && _).addSimulationDelay(1)
  val clickOut = (io.in.req =/= io.in.ack).addSimulationDelay(1)

  withClockAndReset(clickIn.asClock, reset.asAsyncReset) {
    io.in.ack := ToggleReg(0.B)
  }

  withClockAndReset(clickOut.asClock, reset.asAsyncReset) {
    routingTable.map(_._2).zip(io.out).foreach { case (takeRoute, port) =>
      port.req := ToggleReg(0.B, takeRoute)
      port.data := io.in.data
    }
  }




}

class DemuxTop[P <: Data](pgen: P, p: NocParameters, localCoordinate: Coordinate) extends Module {


  val io = IO(new Bundle {
    val in = Flipped(Handshake(new Packet(pgen, p)))
    val out = Vec(4, Handshake(new Packet(pgen, p)))
  })

  val demux = Demux(NorthEast -> io.in, localCoordinate, p)

  println(demux.map(_._1))

  io.out <> demux.map(_._2).toVec



}


object Demux {

  def main(args: Array[String]): Unit = emitVerilog(new DemuxTop(UInt(64.W), NocParameters(8 by 8), Coordinate(8 by 8, 2.U, 3.U)))

  def apply[P <: Data](channel: (Direction, Handshake[Packet[P]]), localCoordinate: Coordinate, p: NocParameters): Seq[(Direction, Handshake[Packet[P]])] = {
    val route = Route(channel._1)
    apply(route.options, channel._2, p, localCoordinate, route.routingTable)
  }

  def apply[P <: Data](n: Int, channel: Handshake[Packet[P]], p: NocParameters, localCoordinate: Coordinate, routingTable: (Header, Coordinate) => Seq[(Direction, Bool)]): Seq[(Direction, Handshake[Packet[P]])] = {
    val module = Module(new Demux(n, chiselTypeOf(channel.data.payload), p, localCoordinate, routingTable))
    module.io.in <> channel
    module.outDirections.zip(module.io.out)
  }


}

