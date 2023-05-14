package noc

import chisel3._
import helpers.Types.GridBuilder
import helpers.{Mapper2D, Zipper2D}

class Adapter[P <: Data](implicit p: NocParameters[P]) extends Module {
  val toNoc = IO(LocalPort())
  val port = IO(LocalPort())

  toNoc.outbound.req := port.inbound.req
  toNoc.outbound.data := port.inbound.data
  port.inbound.ack := toNoc.outbound.ack

  port.outbound.req := toNoc.inbound.req
  port.outbound.data := toNoc.inbound.data
  toNoc.inbound.ack := port.outbound.ack
}


object NOC extends App { emitVerilog(new NOC()(NocParameters(4 by 4, () => UInt(8.W))))}

class NOC[P <: Data](implicit p: NocParameters[P]) extends Module {

  val ports = IO(Vec(p.size.m, Vec(p.size.n, LocalPort())))

  val adapters = Seq.fill(p.size.m, p.size.n)(Module(new Adapter))

  NocBuilder(p, adapters.map2d(_.toNoc))

  ports.zip2d(adapters).map2d { case (port, adapter) =>
    port.outbound.req := adapter.port.outbound.req
    port.outbound.data := adapter.port.outbound.data
    adapter.port.outbound.ack := port.outbound.ack

    adapter.port.inbound.req := port.inbound.req
    adapter.port.inbound.data := port.inbound.data
    port.inbound.ack := adapter.port.inbound.ack
  }


}