
import chisel3._
import chiseltest._
import examples.RingCounterTop
import org.scalatest.flatspec.AnyFlatSpec

class RingCounterTest extends AnyFlatSpec with ChiselScalatestTester {

  "RingCounters" should "count" in {
    test(new RingCounterTop).withAnnotations(Seq(
      IcarusBackendAnnotation,
      WriteVcdAnnotation
    )) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(1000)

    }
  }

}
