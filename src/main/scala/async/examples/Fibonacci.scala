package async.examples

import async._
import async.blocks.FunctionBlock.FunctionBlockExtension
import async.blocks.{Barrier, Fork, HandshakeRegister, HandshakeRegisterNext, Join}
import async.primitives.DelayElement
import chisel3._
import helpers.Hardware.{SetReg, ToggleReg, bruteForceSynchronize, risingEdge}
import helpers.EmitVerilog
import helpers.Types.Pair


class Fibonacci(sim: Boolean) extends Module {
  implicit val conf = DelayElementConfig(sim, () => DelayElement.Xilinx())

  val io = IO(new Bundle {
    val out = Handshake(UInt(8.W))
    val start = Input(Bool())
  })

  val run = SetReg(io.start)

  val r0 = HandshakeRegister(UInt(8.W), Empty)

  val f0 = Fork(2, HandshakeRegisterNext(r0.o, Token(1.U)))

  val f1 = Fork(2, Barrier(HandshakeRegisterNext(f0(0), Token(1.U)), run))
  io.out <> f1(0)

  r0.i <> Join(f0(1), f1(1)).applyFunction(10) { case Pair(a,b) => a + b }

}

class FibonacciTop extends Module {

  val io = IO(new Bundle {
    val out = Handshake(UInt(8.W))
    val ack = Output(Bool())
    val start = Input(Bool())
  })

  val fib = Module(new Fibonacci(false))
  fib.io.out <> io.out

  val toggleAck = ToggleReg(0.B, risingEdge(bruteForceSynchronize(io.out.ack)))
  fib.io.out.ack := toggleAck
  io.ack := toggleAck
  fib.io.start := bruteForceSynchronize(io.start)

}

object Fibonacci extends EmitVerilog(new FibonacciTop)