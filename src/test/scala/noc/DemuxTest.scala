package noc

import async.HandshakeTesting._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import noc.Direction.{East, NorthWest}
import noc.Helper.GridBuilder
import org.scalatest.flatspec.AnyFlatSpec

class DemuxTest extends AnyFlatSpec with ChiselScalatestTester {

  "Demux" should "route packets" in {

    val p = NocParameters(8 by 8)
    val localCoordinate = Coordinate(p.size, 4.U, 3.U)
    val inDir = NorthWest
    val route = Route(inDir)


    test(new Demux(route.options, UInt(8.W), p, localCoordinate, route.routingTable)).withAnnotations(Seq(
     IcarusBackendAnnotation,
      WriteVcdAnnotation
    )) { dut =>

      val outPorts = Map(dut.outDirections.zip(dut.io.out):_*)

      println(outPorts)

      dut.io.in.initSource(dut.clock)
      dut.io.out.foreach(_.initSink(dut.clock))

      fork {
        dut.io.in.send((new Packet(UInt(8.W), p)).Lit(
          _.header.destination -> Coordinate(p.size, 5.U, 3.U),
          _.header.sign.dx -> 1.B,
          _.header.sign.dy -> 1.B,
          _.payload -> 12.U
        ))
      }.fork {
        dut.clock.step(4)
        dut.io.out(2).ack.poke(1.B)
      }.join()

    }
  }

}
