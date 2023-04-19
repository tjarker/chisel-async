package async.examples

import async._
import async.blocks.HandshakeRegisterNext
import chisel3._
import chisel3.internal.firrtl.Width

class Fifo(depth: Int, width: Width) extends Module {

  val io = IO(new Bundle {
    val in = HandshakeIn(UInt(width))
    val out = HandshakeOut(UInt(width))
  })

  io.out <> (0 until depth).foldLeft(io.in) { (acc, _) => HandshakeRegisterNext(acc, Empty) }

}
