package noc

import chisel3._
import chiseltest._
import helpers.Types.{ConstraintBuilder, Coordinate, GridBuilder}
import helpers._
import org.scalatest.flatspec.AnyFlatSpec
import async.HandshakeTesting._
import async.TestingUtils.{ForkExtension, forkForEach}

import scala.util.Random

class RouterTest extends AnyFlatSpec with ChiselScalatestTester {

  implicit val p = NocParameters(8 by 8, () => UInt(8.W))

  Position.all.foreach { pos =>

    val start = Coordinate(p.size, pos)
    val ports = Direction.all.filter(pos.hasPort)
    val options = ports.map { port => port -> RoutingRule(port, pos).options }

    val packets: Seq[(Direction, Seq[(Direction, Seq[Packet[UInt]])])] = options.map { case (inDir, outDirs) =>
      inDir -> outDirs
        .map { outDir =>
          outDir -> Seq.fill(10)(Coordinate(outDir of start))
            .map(c => Packet(start -> c, Random.nextUInt(8.W)))
        }
    }

    val sendPackets = packets.map { case (inDir, outbox) => inDir -> outbox.flatMap(_._2) }
    val receivePackets = ports.map { outDir => outDir -> packets.flatMap(_._2).filter(_._1 == outDir).flatMap(_._2) }

    /*
    packets.foreach { case (inDir, outbox) =>
      outbox.foreach { case (outDir, packets) =>
        packets.foreach { p => assert(sendPackets(inDir).contains(p)) }
        packets.foreach { p => assert(receivePackets(outDir).contains(p)) }
      }
    }

     */


    s"Router@$pos" should s"route packets on ports ${ports.mkString("(", ",", ")")}" in {
      test(new Router(start, pos))
        .withAnnotations(Seq(IcarusBackendAnnotation,WriteVcdAnnotation)) { dut =>

          dut.io.inbound.foreach(_.channel.initSource(dut.clock))
          dut.io.outbound.foreach(_.channel.initSink(dut.clock))

          val inPorts = dut.io.inboundMap
          val outPorts = dut.io.outboundMap

          forkForEach(sendPackets) { case (inDir, packets) =>
            inPorts(inDir).send(packets)
          }.forkForEach(receivePackets) { case (outDir, packets) =>
            outPorts(outDir).receive(packets.length)
          }.join()

      }
    }



  }


}
