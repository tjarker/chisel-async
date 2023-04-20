package noc

import async.HandshakeTesting._
import async.TestingUtils.ForkExtension
import chisel3._
import chiseltest._
import helpers.Types.{ConstraintBuilder, Coordinate, GridBuilder}
import org.scalatest.flatspec.AnyFlatSpec
import helpers._

import scala.util.Random

class DemuxTest extends AnyFlatSpec with ChiselScalatestTester {

  implicit val p = NocParameters(8 by 8, () => UInt(8.W))

  Position.all.foreach { pos =>
    Direction.all.filter(pos.hasPort).foreach { inDir =>

      val start = Coordinate(p.size, pos)
      val route = RoutingRule(inDir, pos)
      val packets = route
        .options
        .map(d => d -> Seq.fill(10)(Coordinate(d of start)))
        .map { case (d,cs) =>
          d -> cs.map(c => Packet(start -> c, Random.nextUInt(8.W)))
        }

      s"Demux@$pos" should s"route packets correctly from $inDir to ${route.options.mkString("(", ", ", ")")}" in {
        test(new Demux(inDir, start, route))
          .withAnnotations(Seq(IcarusBackendAnnotation)) { dut =>

          val outPorts = Map(dut.outDirections.zip(dut.io.out):_*)

          dut.io.in.initSource(dut.clock)
          dut.io.out.foreach(_.initSink(dut.clock))

          fork {
            dut.io.in.send(packets.flatMap(_._2))
          }.forkForEach(packets, Monitor) { case (dir, ps) =>
            outPorts(dir).receiveExpect(ps)
          }.joinAndStep(dut.clock)

        }
      }

    }

  }






}
