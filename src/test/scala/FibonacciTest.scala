
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FibonacciTest extends AnyFlatSpec with ChiselScalatestTester {

  "Fib" should "calc" in {
    test(new Fibonacci()(true)).withAnnotations(Seq(
      IcarusBackendAnnotation,
      WriteVcdAnnotation
    )) { dut =>
      dut.clock.setTimeout(0)

      0 until 1000 foreach { _ =>
        if(dut.io.req.peekBoolean()) {
          dut.clock.step(30)
          dut.io.ack.poke(1.B)
          while(dut.io.req.peekBoolean()) dut.clock.step()
          dut.clock.step()
          dut.io.ack.poke(0.B)
        }
        dut.clock.step()
      }
      dut.reset.poke(1.B)
      dut.clock.step(100)
    }
  }

  "clickFib" should "calc" in {
    test(new FibonacciClick()(true)).withAnnotations(Seq(
      IcarusBackendAnnotation,
      WriteVcdAnnotation
    )) { dut =>
      dut.clock.setTimeout(0)

      var prevReq = false
      var ack = false


      dut.clock.step(30)
      dut.io.start.poke(1.B)

      0 until 1000 foreach { _ =>
        if (dut.io.out.req.peekBoolean() != prevReq) {
          prevReq = !prevReq
          dut.clock.step(20)
          ack = !ack
          dut.io.out.ack.poke(ack.B)
        }
        dut.clock.step()
      }
      dut.reset.poke(1.B)
      dut.clock.step(100)
    }
  }

}