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
  setInline("MutexBox.v",
    s"""module MutexBox(
       |  input req1,
       |  input req2,
       |  output grant1,
       |  output grant2
       |);
       |
       |reg o1 = 1;
       |reg o2 = 1;
       |assign{grant1} = ~o1 & o2;
       |assign{grant2} = ~o2 & o1;
       |
       |always @(*) begin
       |  o2 = #1 ~(req2 & o1);
       |end
       |always @(*) begin
       |  o1 = #1 ~(req1 & o2);
       |end
       |
       |endmodule""".stripMargin)
}

class MutexBoxTest extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val req1= Input(UInt(1.W))
    val req2= Input(UInt(1.W))
    val grant1 = Output(UInt(1.W))
    val grant2 = Output(UInt(1.W))
  })
  setInline("MutexBox.v",
    s"""module MutexBox(
       |  input req1,
       |  input req2,
       |  output grant1,
       |  output grant2
       |);
       |
       |reg o1 = 1;
       |reg o2 = 1;
       |reg del_req1 = 0;
       |reg internal_req2 = 0;
       |assign{grant1} = ~o1 & o2;
       |assign{grant2} = ~o2 & o1;
       |
       |always @(internal_req2 or o1) begin
       |  o2 = #1 ~(internal_req2 & o1);
       |end
       |always @(req1 or o2) begin
       |  o1 = #1 ~(req1 & o2);
       |end
       |
       |always @(req2 or del_req1 or req1) begin
       |  internal_req2 <= #1 !(del_req1 & !req1) & req2;
       |end
       |
       |always @(req1) begin
       |  del_req1 <= #1 req1;
       |end
       |
       |endmodule""".stripMargin)
}

class Mutex extends Module {
    val io = IO(new Bundle {
    val req = Vec(2, Input(UInt(1.W)))
    val grant = Vec(2, Output(UInt(1.W)))
  })
    val m = Module(new MutexBoxTest())
    m.io.req1 := io.req(0)
    m.io.req2 := io.req(1)
    io.grant(0) := m.io.grant1
    io.grant(1) := m.io.grant2
}