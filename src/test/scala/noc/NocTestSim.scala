package noc

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class NocTestSim extends AnyFlatSpec with ChiselScalatestTester {

  "noctest" should "work" in {
    test(new NocTest).withAnnotations(Seq(IcarusBackendAnnotation, WriteVcdAnnotation)) { dut =>

      dut.clock.step(100)

    }
  }

}
