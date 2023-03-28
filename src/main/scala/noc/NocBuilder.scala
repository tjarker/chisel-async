package noc

import chisel3._

object NocBuilder {

  trait NocInterface[P <: Data] {
    val nocIO: Port[P]
  }

  def apply[P <: Data](p: NocParameters[P], modules: Seq[Seq[RawModule with NocInterface[P]]]): Unit = ???

}
