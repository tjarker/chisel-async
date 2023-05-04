package noc

import async.blocks.SimulationDelay.SimulationDelayer
import async.blocks.Sink
import chisel3._
import helpers.BundleExpander
import helpers.Hardware.ToggleReg
import noc.NocBuilder.NocInterface
import helpers._

class Sender(implicit p: NocParameters[UInt]) extends Module with NocInterface[UInt] {

  override val nocIO: Port[UInt] = IO(LocalPort())

  val reqout = Wire(Bool())
  val click = (reqout === nocIO.outbound.ack).addSimulationDelay(1)

  withClockAndReset(click.asClock, reset.asAsyncReset) {
    val x = RegInit(UInt(0 until p.size.n), 1.U)
    val y = RegInit(UInt(0 until p.size.m), 0.U)
    val id = RegInit(0.U(8.W))
    id := id + 1.U
    val finishedRow = x === (p.size.n - 1).U
    val finishedGrid = y === (p.size.m - 1).U
    x := Mux(finishedGrid && finishedRow, 1.U, Mux(finishedRow, 0.U, x + 1.U))
    when(finishedRow) {
      y := Mux(finishedGrid, 0.U, y + 1.U)
    }

    reqout := ToggleReg.init(1.B)
    nocIO.outbound.req := reqout && !(reset.asBool)
    nocIO.outbound.data.expand(
      _.header.sign.dx := 0.B,
      _.header.sign.dy := 0.B,
      _.header.destination.x := x,
      _.header.destination.y := y,
      _.payload := id
    )
  }

   Sink(nocIO.inbound)

}
