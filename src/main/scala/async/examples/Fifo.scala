package async.examples

import async._
import async.blocks.HandshakeRegisterNext
import chisel3._
import chisel3.internal.firrtl.Width

class Fifo(depth: Int, width: Width) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Handshake(UInt(width)))
    val out = Handshake(UInt(width))
  })

  io.out <> (0 until depth).foldLeft(io.in) { (acc, _) => HandshakeRegisterNext(acc, Empty) }

}
