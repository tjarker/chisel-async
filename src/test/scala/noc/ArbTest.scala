package noc

import async.HandshakeTesting._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import helpers.Types.{Coordinate, GridBuilder}
import noc.Direction.{East, NorthWest}
import org.scalatest.flatspec.AnyFlatSpec


class ArbTest extends AnyFlatSpec with ChiselScalatestTester {
//TODO - improve the tests
  "Arbiter" should "arbitrate packets" in {

    val p = NocParameters(8 by 8, () => UInt(8.W))

    test(new Arbiter()(p)).withAnnotations(Seq(
     IcarusBackendAnnotation,
      WriteVcdAnnotation
    )) { dut =>
      dut.io.in(0).req.poke("b0".U)
      dut.io.in(1).req.poke("b0".U)
      dut.clock.step()
      dut.clock.step()
      dut.io.in(0).ack.expect("b0".U)
      dut.io.in(1).ack.expect("b0".U)
      dut.io.out(0).req.expect("b0".U)
      dut.io.in(0).req.poke("b1".U)
//      dut.io.req2.poke("b0".U)
      dut.clock.step(2)
      dut.io.out(0).req.expect("b1".U)
      dut.clock.step(2)
      dut.io.out(0).ack.poke("b1".U)
      dut.clock.step(2)
      dut.io.in(0).ack.expect("b1".U)
      dut.clock.step(2)
      dut.io.in(0).req.expect("b1".U)
      dut.io.in(1).req.expect("b0".U)
      dut.io.in(1).ack.expect("b0".U)
      dut.io.out(0).req.expect("b1".U)
      dut.io.in(1).req.poke("b1".U)
      dut.clock.step(2)
      dut.io.in(0).ack.expect("b1".U)
      dut.clock.step(2)
      dut.io.in(0).req.expect("b1".U)
      dut.io.in(1).req.expect("b1".U)
      dut.io.in(1).ack.expect("b0".U)
      dut.io.out(1).req.expect("b1".U)
      dut.io.out(1).ack.poke("b1".U)
      dut.clock.step()
      dut.io.in(0).ack.expect("b1".U)
      dut.io.in(0).req.expect("b1".U)
      dut.io.in(1).req.expect("b1".U)
      dut.io.in(1).ack.expect("b1".U)
      dut.io.in(0).req.poke("b0".U)
      dut.clock.step()
      dut.io.in(0).ack.expect("b1".U)
      dut.io.in(0).req.expect("b0".U)
      dut.io.in(1).req.expect("b1".U)
      dut.clock.step(2)
      dut.io.in(1).ack.expect("b1".U)
      dut.io.out(0).req.expect("b0".U)
    }
  }

}
