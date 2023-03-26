package noc

import async.HandshakeTesting._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import helpers.Types.{Coordinate, GridBuilder}
import noc.Direction.{East, NorthWest}
import org.scalatest.flatspec.AnyFlatSpec

class MutexTest extends AnyFlatSpec with
  ChiselScalatestTester {

  "Mutex" should "arbitrate outputs" in {

    test(new Mutex) { dut =>
        dut.io.req1.poke("b1".U)
        dut.io.req2.poke("b0".U)
        dut.clock.step()
        dut.io.grant1.expect("b1".U)


    }
  }

}
