package noc


import async.Handshake
import chisel3._
import noc.Helper.GridBuilder

trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

class Demux[P <: Data](pgen: P, fs: Seq[(Direction, Header => Bool)]) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(new Packet(pgen,NocParameters(2 by 2))))
    val out = Vec(fs.size, Handshake(new Packet(pgen, NocParameters(2 by 2))))
  })

  val outDirections = fs.map(_._1)

  fs.zip(io.out).foreach { case ((direction, headerToBool), out) =>
    out.req := io.in.req && headerToBool(io.in.payload.header)
    out.payload := io.in.payload
    //io.in.req := out.ack
  }

}
object Demux {

  def apply[P <: Data](channel: Handshake[Packet[P]], fs: Seq[(Direction, Header => Bool)]): Seq[(Direction, Handshake[Packet[P]])] = {
    val module = Module(new Demux(chiselTypeOf(channel.payload.payload), fs))
    module.io.in := channel
    module.outDirections.zip(module.io.out)
  }


}
