package async

import chisel3._
import chisel3.experimental.requireIsHardware
import chisel3.internal.firrtl.Width
import chisel3.util.log2Ceil

import scala.language.implicitConversions


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

  implicit def RangeToWidth(x: Range): Width = log2Ceil(x.max + 1).W

  implicit class UIntFactoryExtension(x: chisel3.UInt.type) {
    def apply(range: Range): UInt = UInt(log2Ceil(range.max + 1).W)
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

  def risingEdge(x: Bool) = x && !RegNext(x, 0.B)

  object ToggleReg {
    def apply(init: Bool, event: Bool = 1.B): Bool = {
      val toggleReg = RegInit(init)
      when(event) { toggleReg := !toggleReg }
      toggleReg
    }
  }

  object SetReg {
    def apply(event: Bool): Bool = {
      val setReg = RegInit(0.B)
      when(event) { setReg := 1.B }
      setReg
    }
  }

}
