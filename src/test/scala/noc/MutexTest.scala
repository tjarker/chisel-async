package noc

import async.HandshakeTesting._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import helpers.Types.{Coordinate, GridBuilder}
import noc.Direction.{East, NorthWest}
import org.scalatest.flatspec.AnyFlatSpec

class MutexTest extends AnyFlatSpec with
  ChiselScalatestTester{

  "Mutex" should "arbitrate outputs" in {

    test(new Mutex).withAnnotations(Seq(
      IcarusBackendAnnotation,
      WriteVcdAnnotation
    )) { dut =>
        dut.io.req(0).poke("b1".U)
        dut.io.req(1).poke("b0".U)
        dut.clock.step()
        dut.io.grant(0).expect("b1".U)
        dut.io.grant(1).expect("b0".U)
        dut.io.req(0).poke("b0".U)
        dut.io.req(1).poke("b1".U)
        dut.clock.step()
        dut.io.grant(0).expect("b0".U)
        dut.io.grant(1).expect("b1".U)
    }
  }

}
