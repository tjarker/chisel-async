package async.blocks

import async.DelayElementConfig
import async.primitives.DelayElement
import chisel3._



private class Delay(delay: Int, delayElementFactory: () => DelayElement) extends RawModule {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })

  io.out := (0 until delay).foldLeft(io.in) { (acc, _) =>
    val m = Module(delayElementFactory())
    m.io.in := acc
    m.io.out
  }

}



object Delay {
  def apply(x: Bool, delay: Int)(implicit delayElementConfig: DelayElementConfig): Bool = if(delayElementConfig.simulation) {
    SimulationDelay(x, delay)
  } else {
    val mod = Module(new Delay(delay, delayElementConfig.delayElementFactory))
    mod.io.in := x
    mod.io.out
  }

  implicit class BoolDelayer(x: Bool) {
    def addDelay(delay: Int)(implicit delayElementConfig: DelayElementConfig): Bool =
      if(delayElementConfig.simulation) SimulationDelay(x, delay) else Delay(x, delay)
  }

}