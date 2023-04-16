package async.examples


import async.HandshakeTesting.HandshakeDriver
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FibonacciTest extends AnyFlatSpec with ChiselScalatestTester {

  "Fibonacci" should "produce fibonacci sequence" in {
    test(new Fibonacci(true))
      .withAnnotations(Seq(IcarusBackendAnnotation, WriteVcdAnnotation)) { dut =>

      dut.clock.setTimeout(10000)
      dut.io.out.initSink(dut.clock)

      dut.io.start.poke(1.B)

      dut.io.out.receiveExpect(fibonacciSeq(13).map(_.U))

    }
  }

  def fibonacciSeq(n: Int): Seq[Int] = Seq.tabulate(n)(fibonacci)
  def fibonacci(n: Int): Int = n match {
    case 0 | 1 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)
  }
}