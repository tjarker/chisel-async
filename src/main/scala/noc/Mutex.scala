package noc
import async.Handshake
import chisel3._
import helpers.Hardware.ToggleReg
import helpers.TreeReducer
import noc.Channel.OutboundChannel
import chisel3.util.HasBlackBoxInline
class MutexBox extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val req1= Input(UInt(1.W))
    val req2= Input(UInt(1.W))
    val grant1 = Output(UInt(1.W))
    val grant2 = Output(UInt(1.W))
  })
  setInline("module MutexBox.v",
    s"""module MutexBox(
       |  input req1,
       |  input req2,
       |  output grant1,
       |  output grant2
       |);
       |
       |wire o1 = ~(req1 & o2);
       |wire o2 = ~(req2 & o1);
       |assign{grant1} = ~o1 & o2;
       |assign{grant2} = ~o2 & o1;
       |
       |endmodule""".stripMargin)
}

class Mutex extends Module {
    val io = IO(new Bundle {
    val req = Vec(2, Input(UInt(1.W)))
    val grant = Vec(2, Output(UInt(1.W)))
  })
    val m = Module(new MutexBox())
    m.io.req1 := io.req(0)
    m.io.req2 := io.req(1)
    io.grant(0) := m.io.grant1
    io.grant(1) := m.io.grant2
}
object Mutex extends App{
  emitVerilog (new Mutex())
}
