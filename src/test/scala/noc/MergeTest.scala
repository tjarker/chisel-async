package noc

import async.HandshakeTesting._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import helpers.Types.{Coordinate, GridBuilder}
import noc.Direction.{East, NorthWest}
import org.scalatest.flatspec.AnyFlatSpec


class MergeTest extends AnyFlatSpec with ChiselScalatestTester {

  "Merge" should "merge packets" in {

    val p = NocParameters(8 by 8, () => UInt(8.W))

    test(new Merge()(p)).withAnnotations(Seq(
     IcarusBackendAnnotation,
      WriteVcdAnnotation
    )) { dut =>
      dut.io.in(0).req.poke("b0".U)
      dut.io.in(1).req.poke("b0".U)
      dut.clock.step()
      dut.io.in(0).ack.expect("b0".U)
      dut.io.in(1).ack.expect("b0".U)
      dut.io.out.req.expect("b0".U)
      dut.io.in(0).req.poke("b1".U)
//      dut.io.req2.poke("b0".U)
      dut.clock.step()
      dut.io.out.req.expect("b1".U)
      dut.io.out.ack.poke("b1".U)
      dut.clock.step()
      dut.io.in(0).ack.expect("b1".U)
      dut.io.in(0).req.expect("b1".U)
      dut.io.in(1).req.expect("b0".U)
      dut.io.in(1).ack.expect("b0".U)
      dut.io.out.req.expect("b1".U)
      dut.io.in(1).req.poke("b1".U)
      dut.clock.step()
      dut.io.in(0).ack.expect("b1".U)
      dut.io.in(0).req.expect("b1".U)
      dut.io.in(1).req.expect("b1".U)
      dut.io.in(1).ack.expect("b0".U)
      dut.io.out.req.expect("b0".U)
    }
  }

}
