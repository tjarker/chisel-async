package noc

import chiseltest._
import noc.examples.NocTest
import org.scalatest.flatspec.AnyFlatSpec

class NocTestSim extends AnyFlatSpec with ChiselScalatestTester {

  "noctest" should "work" in {
    test(new NocTest).withAnnotations(Seq(IcarusBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(1000)

    }
  }

}
