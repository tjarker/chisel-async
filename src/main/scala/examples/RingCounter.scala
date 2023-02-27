package examples

import chisel3._
import fourPhaseBundledData.HandshakeLatch
import handshake.Handshake
import handshake.HandshakeInitializer.{Empty, ValidToken}


class RingCounterTop extends Module {

  val io = IO(Vec(10, Handshake(UInt(8.W))))

  io.zipWithIndex.foreach { case (port, i) =>
    port <> Module(new RingCounter(i + 2)).out
  }

}


class RingCounter(n: Int) extends Module {

  val out = IO(Handshake(UInt(8.W)))

  val start = HandshakeLatch(UInt(8.W), ValidToken(0.U))
  out <> start.io.out

  val end = (0 until n - 1).foldLeft(start) { (acc, _) =>
    acc ||> HandshakeLatch(UInt(4.W), Empty)
  }
  end ||> start
  start.io.in.payload := end.io.out.payload + 1.U
}