package noc
import async.{Handshake, HandshakeIn, HandshakeOut}
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.Hardware.SetRegWithValue
import helpers.TreeReducer
import noc.Channel.OutboundChannel
import async.blocks.SimulationDelay.SimulationDelayer
//package async.blocks
import chisel3.util.HasBlackBoxInline
import helpers.Types.{Coordinate, GridBuilder}

class Merge[P <: Data]()(implicit p:NocParameters[P]) extends Module {
  val io = IO(new Bundle {
    val in = Vec(2, HandshakeIn(Packet()))
    val out = HandshakeOut(Packet())
  })
  //new request detected
  val sela = io.in(0).ack  ^ io.in(0).req
  val clickOut = sela | (io.in(1).ack ^ io.in(1).req)
    .addSimulationDelay(1)
  //send request out when new request processed
  withClockAndReset(clickOut.asClock, reset.asAsyncReset) {
    io.out.req := ToggleReg.init(0.B)
    io.out.data := Mux(sela, io.in(0).data, io.in(1).data)
  }
  //ack received after request was sent
  val clickIn = !(io.out.ack ^ io.out.req)
    .addSimulationDelay(1)
  //create ack nodes
  val ackNode = Wire(Vec(2, UInt(1.W)))
  //send ack when received ack
  withClockAndReset(clickIn.asClock, reset.asAsyncReset) {
    ackNode.zip(io.in).map { case (ackout, in) => {
      in.ack := ackout
      ackout := SetRegWithValue(in.req, 1.B)
      }
    }
  }
}

class Arbiter[P <: Data]()(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, HandshakeIn(Packet()))
    val out = Vec(2, HandshakeOut(Packet()))
  })

  val m = Module(new Mutex())

  val grantReleased = Wire(Vec(2,UInt(1.W)))
  val clickOut = Wire(Vec(2, UInt(1.W)))
  val prevState = Wire(Vec(2, UInt(1.W)))
  val clickIn = Wire(Vec(2, UInt(1.W)))

    for (i <- 0 until 2) {
      //click if mutex grants
      clickIn(i) := m.io.grant(i)
        .addSimulationDelay(1)
      withClockAndReset(clickIn(i)(0).asClock, reset.asAsyncReset) {
        //send feedback about request
        grantReleased(i) := ToggleReg.init(0.B)
        //send request to the output
        io.out(i).req := ToggleReg.init(0.B)
      }
      //is there a new request on the input?
      clickOut(i) := ((!grantReleased(i) & io.in(i).req & !prevState(i)) | (grantReleased(i) & !io.in(i).req & prevState(i)))
        .asBool
        .addSimulationDelay(1)

      withClockAndReset(clickOut(i)(0).asClock, reset.asAsyncReset) {
        //save current state for future request check
        prevState(i) := ToggleReg.init(0.B)
      }
      //request to mutex if input request changed
      m.io.req(i) := io.out(i).ack ^ prevState(i)
      io.in(i).ack := io.out(i).ack
      io.out(i).data := io.in(i).data
    }

}


class ArbMerge[P <: Data]()(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, HandshakeIn(Packet()))
    val out = HandshakeOut(Packet())
  })
  val arbiter = Module(new Arbiter()(p))
  val merge = Module(new Merge()(p))

  io.out.req := merge.io.out.req
  io.out.data := merge.io.out.data
  merge.io.out.ack := io.out.ack

  arbiter.io.in(0).req := io.in(0).req
  arbiter.io.in(1).req := io.in(1).req
  arbiter.io.in(0).data := io.in(0).data
  arbiter.io.in(1).data := io.in(1).data
  io.in(0).ack := arbiter.io.in(0).ack
  io.in(1).ack := arbiter.io.in(1).ack

  merge.io.in(0).req := arbiter.io.out(0).req
  merge.io.in(1).req := arbiter.io.out(1).req
  merge.io.in(0).data := arbiter.io.out(0).data
  merge.io.in(1).data := arbiter.io.out(1).data
  arbiter.io.out(0).ack := merge.io.in(0).ack
  arbiter.io.out(1).ack := merge.io.in(1).ack
}

class ArbMergeTree[P <: Data](n: Int)(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Vec(n, HandshakeIn(Packet()))
    val out = HandshakeOut(Packet())
  })

  io.out <> io.in.reduceTree { (l, r) =>
    val arbMerge = Module(new ArbMerge())
    arbMerge.io.in(0) <> l
    arbMerge.io.in(1) <> r
    arbMerge.io.out
  }

}

object ArbMerge {

  def apply[P <: Data](channels: Seq[OutboundChannel[P]])(implicit p: NocParameters[P]): OutboundChannel[P] = {
    channels match {
      case Seq(channel) => channel
      case _ =>
        require(channels.tail.forall(_.heading == channels.head.heading))
        val arbMergeTree = Module(new ArbMergeTree(channels.length))
        arbMergeTree.io.in.zip(channels).foreach { case (port, channel) => port <> channel.channel }
        OutboundChannel(channels.head.heading, arbMergeTree.io.out)
    }

  }

}
