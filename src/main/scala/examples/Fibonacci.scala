package examples

import util.Helper.{Pair, synchronize, toggle}
import chisel3._
import click.{Barrier, HandshakeRegister}
import fourPhaseBundledData.{Fork, FunctionBlock, HandshakeLatch, Join}
import handshake.Handshake
import handshake.HandshakeInitializer.{Empty, ValidToken}
import primitives.DelayElement


class Fibonacci(implicit sim: Boolean) extends Module {
  implicit val delayElement = () => DelayElement.Xilinx()

  val io = IO(Handshake(UInt(8.W)))

  val l0 = HandshakeLatch(UInt(8.W), Empty)
  val l2 = l0 ||> HandshakeLatch(UInt(8.W), Empty) ||> HandshakeLatch(UInt(8.W), ValidToken(1.U))

  val f0 = Fork(l2.io.out)(2)

  val l3 = HandshakeLatch(UInt(8.W), Empty)
  l3.io.in <> f0(0)
  val l5 = l3 ||> HandshakeLatch(UInt(8.W), Empty) ||> HandshakeLatch(UInt(8.W), ValidToken(1.U))

  val f1 = Fork(l5.io.out)(2)
  io <> f1(0)

  l0.io.in <> FunctionBlock(Join(f0(1), f1(1)), 50) { case Pair(a,b) => a + b }

}

class FibonacciTop extends Module {

  val io = IO(Handshake(UInt(8.W)))
  val ack = IO(Output(Bool()))

  val fib = Module(new Fibonacci()(false))
  fib.io <> io
  val toggleAck = toggle(io.ack)
  fib.io.ack := toggleAck
  ack := toggleAck

}

class FibonacciClick(implicit sim: Boolean) extends Module {
  implicit val delayElement = () => DelayElement.Xilinx()

  val io = IO(new Bundle {
    val out = Handshake(UInt(8.W))
    val start = Input(Bool())
  })

  val run = RegInit(0.B)
  when(synchronize(io.start)) { run := 1.B }

  val l0 = HandshakeRegister(UInt(8.W), Empty)
  val l2 = l0 ||> HandshakeRegister(UInt(8.W), Empty) ||> HandshakeRegister(UInt(8.W), ValidToken(1.U))

  val f0 = click.Fork(Barrier(l2.io.out, run))(2)

  val l3 = HandshakeRegister(UInt(8.W), Empty)
  l3.io.in <> f0(0)
  val l5 = l3 ||> HandshakeRegister(UInt(8.W), Empty) ||> HandshakeRegister(UInt(8.W), ValidToken(1.U))

  val f1 = click.Fork(Barrier(l5.io.out, run))(2)
  io.out <> f1(0)

  l0.io.in <> FunctionBlock(click.Join(f0(1), f1(1)), 10) { case Pair(a,b) => a + b }

}

class FibonacciClickTop extends Module {

  val io = IO(new Bundle {
    val out = Handshake(UInt(8.W))
    val ack = Output(Bool())
    val start = Input(Bool())
  })


  val fib = Module(new FibonacciClick()(false))
  fib.io.out <> io.out
  val toggleAck = toggle(io.out.ack)
  fib.io.out.ack := toggleAck
  io.ack := toggleAck
  fib.io.start := io.start

}

object Fibonacci extends App { emitVerilog(new FibonacciClickTop) }