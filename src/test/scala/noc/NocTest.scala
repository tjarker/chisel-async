package noc

import async.HandshakeTesting.HandshakeDriver
import chisel3._
import chiseltest._
import helpers.Types.{Coordinate, Grid, GridBuilder}
import helpers._
import async.TestingUtils._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.stage.PrintFullStackTraceAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer

class NocTest extends AnyFlatSpec with ChiselScalatestTester {

  class Letter(size: Grid) extends Bundle {
    val senderCoord = Coordinate(size)
    val messageId = UInt(32.W)
  }

  for(m <- 2 to 5) {
    for(n <- 2 to 5) {

      val size = n by m

      implicit val p = NocParameters(size, () => new Letter(size))


      s"${n}x${m} Noc" should "send letters" in {
        test(new NOC()).withAnnotations(Seq(IcarusBackendAnnotation, WriteVcdAnnotation, PrintFullStackTraceAnnotation)) { dut =>

          dut.ports.flatten.map(_.outbound).foreach(_.initSink(dut.clock))
          dut.ports.flatten.map(_.inbound).foreach(_.initSource(dut.clock))
          dut.clock.setTimeout(10000)

          val runVec = ArrayBuffer.fill(size.m, size.n)(false)

          val allSent = ArrayBuffer.fill(size.m, size.n)(false)
          val allReceived = ArrayBuffer.fill(size.m, size.n)(false)

          forkForEach2d(dut.ports.map2d(_.inbound).zipWithIndex2d) { case (channel, (i, j)) =>
            println(s"$channel $i, $j")
            var messageId = 0
            val nodeId = (i << (24)) | (j << (16))
            for (y <- 0 until size.m) {
              for (x <- 0 until size.n) {
                if (x != i || y != j) {
                  //println(s"($i,$j) -> ($x,$y)")
                  channel.send(Packet(Coordinate(size, i.U, j.U) -> Coordinate(size, x.U, y.U), (new Letter(size)).Lit(
                    _.senderCoord -> Coordinate(size, i.U, j.U),
                    _.messageId -> {
                      messageId += 1; nodeId | (messageId % 0xFFFF)
                    }.U
                  )))
                }
              }
            }
            allSent(j)(i) = true
            println("-------------------\n"+allSent.map(_.map(if(_) "!" else "_").mkString(" ")).mkString("\n")+"\n-------------------")
            while(!runVec.flatten.reduce(_ && _)) dut.clock.step()
          }.forkForEach2d(dut.ports.map2d(_.outbound).zipWithIndex2d, Monitor) { case (channel, (x, y)) =>

            val expected = Position.coordinateGrid(size).map2d { case (_, c) =>
              val (i,j) = c.toTuple
              if(x != i || y != j) Some((new Packet).Lit(
                _.header.destination.x -> x.U,
                _.header.destination.y -> y.U,
                _.payload.senderCoord.x -> i.U,
                _.payload.senderCoord.y -> j.U
              )) else None
            }.flatten.collect { case Some(p) => p }

            channel.receiveExpectUnordered(expected, comp, p => s"@($x,$y): ${p.payload.senderCoord.toString()} -> ${p.header.destination.toString()} (${p.payload.messageId.litValue.toString(16)})", x==0 && y==2)

            runVec(y)(x) = true
            allReceived(y)(x) = true
            println("-------------------\n"+allReceived.map(_.map(if(_) "*" else "_").mkString(" ")).mkString("\n")+"\n-------------------")

          }.join()
        }
      }
    }
  }

  def comp(a: Packet[Letter], b: Packet[Letter]): Boolean = {
    (a.header.destination == b.header.destination) && (a.payload.senderCoord == b.payload.senderCoord)
  }


}
