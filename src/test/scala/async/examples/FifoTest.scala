package async.examples

import async.HandshakeTesting._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FifoTest extends AnyFlatSpec with ChiselScalatestTester {

  "Fifo" should "forward tokens" in {
    test(new Fifo(4, 8.W))
      .withAnnotations(Seq(IcarusBackendAnnotation)) { dut =>

      dut.clock.setTimeout(10000)
      dut.io.in.initSource(dut.clock)
      dut.io.out.initSink(dut.clock)

      fork {
        dut.io.in.send(0 until 10 map(_.U))
      }.fork {
        dut.io.out.receiveExpect(0 until 10 map(_.U))
      }.join()

      parallel(
        dut.io.in.send(200.U, 99.U),
        dut.io.out.receiveExpect(200.U, 99.U)
      )

    }
  }

}
