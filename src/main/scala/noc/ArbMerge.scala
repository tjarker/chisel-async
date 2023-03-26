package noc
import async.Handshake
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.TreeReducer
import noc.Channel.OutboundChannel
//package async.blocks
import chisel3.util.HasBlackBoxInline
import helpers.Types.{Coordinate, GridBuilder}

class Merge[P <: Data]()(implicit p:NocParameters[P]) extends Module {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Handshake(Packet())))
    val out = Handshake(Packet())
  })
  //new request detected
  val sela = io.in(0).ack  ^ io.in(0).req
  val oneof = sela | io.in(1).ack ^ io.in(1).req

  //send request out when new request processed
  withClockAndReset(oneof.asClock, reset.asAsyncReset) {
    io.out.req := ToggleReg(0.B)
    io.out.data := Mux(sela, io.in(0).data, io.in(1).data)
  }

  //ack received after request was sent
  val done = !(io.out.ack ^ io.out.req)
  //create ack nodes
  val ack2Node = Wire(UInt(1.W))
  val ack1Node = Wire(UInt(1.W))
  io.in(0).ack := ack1Node
  io.in(1).ack := ack2Node
  //send ack when received ack
  withClockAndReset(done.asClock, reset.asAsyncReset) {
    //TODO:simplify into one
    val ack1Reg = RegInit(0.B)
    ack1Reg := io.in(0).req
    ack1Node := ack1Reg

    val ack2Reg = RegInit(0.B)
    ack2Reg := io.in(1).req
    ack2Node := ack2Reg
  }

}

class Arbiter[P <: Data]()(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Handshake(Packet())))
    val out = Vec(2, Handshake(Packet()))
  })

  val m = Module(new Mutex())

  val grantReleased = Wire(Vec(2,UInt()))
  val reqChange = Wire(Vec(2, UInt()))
  val prevState = Wire(Vec(2, UInt()))
  val rout = Wire(Vec(2, UInt(2.W)))

  //TODO : simplify the bottom code into one (maybe with for loop?) - merge upper and lower path - and add comments

//    for (i <- 0 until 1) {
//      rout(i) := m.io.grant(i)
//      withClockAndReset(rout(i)(0).asClock, reset.asAsyncReset) {
//        grantReleased(i) := ToggleReg(0.B)
//        io.out(i).req := ToggleReg(0.B)
//      }
//
//      reqChange(i) := (!grantReleased(i) & io.in(i).req & !prevState(i)) | (grantReleased(i) & !io.in(i).req & prevState(i))
//
//      withClockAndReset(reqChange(i)(0).asClock, reset.asAsyncReset) {
//        prevState(i) := ToggleReg(0.B)
//      }
//      m.io.req(i) := io.out(i).ack ^ prevState(i)
//      io.in(i).ack := io.out(i).ack
//      io.out(i).data := io.in(i).data
//    }
  //upper path
  //request if mutex grants
  rout(0) := m.io.grant(0)
  withClockAndReset(rout(0)(0).asClock, reset.asAsyncReset) {
    //send feedback about request
    grantReleased(0) :=  ToggleReg(0.B)
    //send request to output
    io.out(0).req := ToggleReg(0.B)
  }
  //is there a new request on the input?
  reqChange(0) := (!grantReleased(0) & io.in(0).req & !prevState(0)) | (grantReleased(0) & !io.in(0).req & prevState(0))

  withClockAndReset(reqChange(0)(0).asClock, reset.asAsyncReset) {
    //safe current state for future request check
    prevState(0) := ToggleReg(0.B)
  }
  //request to mutex if input request changed
  m.io.req(0) := io.out(0).ack ^ prevState(0)
  io.in(0).ack := io.out(0).ack
  io.out(0).data := io.in(0).data

  //lower path - TODO - merge with upper path
  rout(1) := m.io.grant(1)
  withClockAndReset(rout(1)(0).asClock, reset.asAsyncReset) {
    grantReleased(1) := ToggleReg(0.B)
    io.out(1).req := ToggleReg(0.B)
  }
  reqChange(1) := (!grantReleased(1) & io.in(1).req & !prevState(1)) | (grantReleased(1) & !io.in(1).req & prevState(1))

  withClockAndReset(reqChange(1)(0).asClock, reset.asAsyncReset) {
    prevState(1) := ToggleReg(0.B)
  }
  m.io.req(1) := io.out(1).ack ^ prevState(1)
  io.in(1).ack := io.out(1).ack
  io.out(1).data := io.in(1).data
}


class ArbMerge[P <: Data]()(implicit p: NocParameters[P]) extends Module {

  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Handshake(Packet())))
    val out = Handshake(Packet())
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
    val in = Vec(n, Flipped(Handshake(Packet())))
    val out = Handshake(Packet())
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
