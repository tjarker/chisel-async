package noc
import async.HandshakeTesting.HandshakeDriver
import async.HandshakeTesting._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import helpers.Types.{Coordinate, GridBuilder}
import org.scalatest.flatspec.AnyFlatSpec
import async.blocks.{Sink}


class ArbMergeTest extends AnyFlatSpec with ChiselScalatestTester {
  //TODO - improve the tests
  "ArbiterMerge" should "arbitrate and merge packets" in {

    val p = NocParameters(8 by 8, () => UInt(8.W))

    test(new ArbMerge()(p)).withAnnotations(Seq(
      IcarusBackendAnnotation,
      WriteVcdAnnotation
    )){ dut =>
      dut.clock.setTimeout(60000)
      dut.clock.step(3)

      //      val si = new Sink()
      dut.io.out.initSink(dut.clock)
//      dut.io.in(0).initSource(dut.clock)
      dut.io.out.req.expect(0.B)
      dut.io.in.foreach(_.initSource(dut.clock))
//      dut.io.out.setSinkClock(dut.clock)
      //      dut.io.in(1).initSource(dut.clock)
      val sequence1 = Seq.fill(3)(new Packet()(p).Lit(
        _.header.destination -> Coordinate(p.size, 3.U, 4.U),
        _.header.sign.dx -> 1.B,
        _.header.sign.dy -> 1.B,
        _.payload -> 12.U
      ))
      val sequence2 = Seq.fill(3)(new Packet()(p).Lit(
        _.header.destination -> Coordinate(p.size, 2.U, 2.U),
        _.header.sign.dx -> 1.B,
        _.header.sign.dy -> 1.B,
        _.payload -> 2.U
      ))
      fork {
      dut.io.in(0).send(sequence1)
      }.fork {
        dut.io.in(1).send(sequence2)
      }.fork {
          dut.clock.step(4)
          //        dut.io.out.data.expect(sequence1(0))
          dut.io.out.ack.poke(1.B)
          dut.clock.step(4)
          //        dut.io.out.data.expect(sequence1(1))
          dut.io.out.ack.poke(0.B)
          dut.clock.step(4)
          //        dut.io.out.data.expect(sequence1(2))
          dut.io.out.ack.poke(1.B)
          dut.clock.step(4)
          dut.io.out.ack.poke(0.B)
          dut.clock.step(4)
          dut.io.out.ack.poke(1.B)
          dut.clock.step(4)
          dut.io.out.ack.poke(0.B)
          dut.clock.step(4)
        }.join()


//      dut.io.out.receiveExpect(sequence2(0))
//      dut.io.out.receiveExpect(sequence1(1))
    }
  }

}
