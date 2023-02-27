package util


import chisel3._
import chisel3.experimental.requireIsHardware

object Helper {

  implicit class BooleanToInt(x: Boolean) { def toInt: Int = if(x) 1 else 0 }

  implicit class OptionExtension[A](x: Option[A]) {
    def andThen[B](f: A => B): Option[B] = x match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  implicit class BundleExpander[T <: Bundle](b: T) {
    def expand(assignments: T => Any*): T = {
      assignments.foreach(f => f(b))
      b
    }
  }

  implicit class SeqToChiselVec[T <: Data](x: Seq[T]) {
    def toVec = VecInit(x)
  }

  class Pair[A <: Data, B <: Data](t1: A, t2: B) extends Bundle {
    val _1 = t1
    val _2 = t2
  }

  object Pair {
    def apply[A <: Data, B <: Data](t1: A, t2: B): Pair[A, B] = {
      try {
        requireIsHardware(t1)
        requireIsHardware(t2)
        Wire(new Pair(chiselTypeOf(t1), chiselTypeOf(t2))).expand(
          _._1 := t1,
          _._2 := t2
        )
      } catch {
        case e: ExpectedHardwareException => new Pair(t1, t2)
      }
    }

    def unapply[A <: Data, B <: Data](p: Pair[A, B]): Option[(A, B)] = Some(p._1 -> p._2)
  }


  def synchronize[T <: Data](x: T): T = RegNext(RegNext(x, 0.U.asTypeOf(x)), 0.U.asTypeOf(x))

  def toggle(x: Bool): Bool = {

    val synced = synchronize(x)
    val toggler = RegInit(0.B)

    when(!RegNext(synced, 0.B) && synced) { toggler := !toggler }

    toggler

  }

  object ToggleReg {
    def apply(init: Bool): Bool = {
      val toggleReg = RegInit(init)
      toggleReg := !toggleReg
      toggleReg
    }
  }

}
