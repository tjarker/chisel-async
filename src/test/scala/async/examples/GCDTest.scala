package async.examples


import async.HandshakeTesting.HandshakeDriver
import chisel3._
import chiseltest._
import helpers.Types.Pair
import org.scalatest.flatspec.AnyFlatSpec

class GCDTest extends AnyFlatSpec with ChiselScalatestTester {

  "GCD" should "determine greatest common divisor" in {
    test(new GCD(32.W,true))
      .withAnnotations(Seq(IcarusBackendAnnotation, WriteVcdAnnotation)) { dut =>

        dut.clock.setTimeout(10000)
        dut.io.operands.initSource(dut.clock)
        dut.io.result.initSink(dut.clock)

        fork {
          val p = Pair(923840450.U(32.W), 2083902834.U(32.W))
          dut.io.operands.send(p)
        }.fork {
          dut.io.result.receiveExpect(26.U)
        }.join()

      }
  }
}