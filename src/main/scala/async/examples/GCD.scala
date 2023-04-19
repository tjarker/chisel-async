package async.examples

import async.blocks.FunctionBlock.FunctionBlockExtension
import async.{DelayElementConfig, Empty, HandshakeIn, HandshakeOut, Token}
import async.blocks._
import async.primitives.DelayElement
import chisel3._
import chisel3.internal.firrtl.Width
import helpers.Types.Pair

class GCD(w: Width, sim: Boolean = false) extends Module {
  implicit val conf = DelayElementConfig(sim, () => DelayElement.Xilinx())

  val io = IO(new Bundle {
    val operands = HandshakeIn(Pair(UInt(w), UInt(w)))
    val result = HandshakeOut(UInt(w))
  })

  // create handshake registers
  val terminationCheckReg = RegFork(Pair(UInt(w), UInt(w)), Empty)
  val iterationReg = RegFork(Pair(UInt(w), UInt(w)), Empty)
  val inputSelect = HandshakeRegister(Bool(), Token(0.B))

  // check whether operands are equal
  val notEqual = Fork(2, terminationCheckReg.io.out._1.applyFunction(2) { case Pair(a, b) => a =/= b })
  // connect result to input select handshake register
  inputSelect.in <> notEqual(0)

  // forward result to output or into the next iteration based on termination condition
  val out = Demux(notEqual(1), terminationCheckReg.io.out._2)
  // connect output to either a or b
  io.result <> out._1.applyFunction(0) { case Pair(a, _) => a }
  // connect operands to iteration stage handshake register
  iterationReg.io.in <> out._2

  // determine whether a or b should be updated
  val pathSelect = iterationReg.io.out._1.applyFunction(10) { case Pair(a, b) => a > b }
  val paths = Demux(pathSelect, iterationReg.io.out._2)

  // calculate the new state
  val changeAPath = paths._1.applyFunction(10) { case Pair(a, b) => Pair(a, b - a) }
  val changeBPath = paths._2.applyFunction(10) { case Pair(a, b) => Pair(a - b, b) }

  // merge the results
  val iterationResult = Merge(Pair(changeAPath, changeBPath))

  // connect the iteration result and input to the termination check stage
  terminationCheckReg.io.in <> HMux(inputSelect.out, iterationResult, io.operands)
}

object GCD extends App { emitVerilog(new GCD(8.W)) }
