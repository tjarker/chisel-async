package noc.examples

import async.HandshakeOut
import async.blocks.SimulationDelay.SimulationDelayer
import async.blocks.Sink
import chisel3._
import helpers.Hardware.ToggleReg
import helpers._
import helpers.Types.GridBuilder
import noc.NocBuilder.NocInterface
import noc._

object NocTest extends App { emitVerilog(new NocTest) }

class NocTest extends Module {

  implicit val p = NocParameters(3 by 3, () => UInt(8.W))

  val io = IO(HandshakeOut(Packet()))

  val dummies = (Module(new Sender) +: Seq.fill(2)(Module(new Dummy))) +: Seq.fill(2,3)(Module(new Dummy))

  val routers = NocBuilder(p, dummies.map2d(_.nocIO))

  io.req := routers(0)(0).io.local.get.outbound.req
  io.data := routers(0)(0).io.local.get.outbound.data

}

class Dummy()(implicit p: NocParameters[UInt]) extends Module with NocInterface[UInt]  {
  override val nocIO: Port[UInt] = IO(LocalPort())

  val sink = Sink(nocIO.inbound)
  val click = nocIO.inbound.req =/= nocIO.inbound.ack
  withClockAndReset(click.asClock, reset.asAsyncReset) {
    val reg = RegNext(nocIO.inbound.data)
    nocIO.outbound.data := reg
  }

  nocIO.outbound.req := 0.B


}

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

