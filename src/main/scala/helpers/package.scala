
import chisel3.internal.firrtl.Width
import chisel3.util.log2Ceil
import chisel3._
import firrtl.AnnotationSeq

import scala.language.implicitConversions

package object helpers {


  implicit class BooleanToInt(x: Boolean) {
    def toInt: Int = if (x) 1 else 0
  }

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



  class EmitVerilog(gen: => RawModule, args: Array[String] = Array.empty, annotations: AnnotationSeq = Seq.empty) extends App {
    emitVerilog(gen, args, annotations)
  }


  def reduce[T <: Data](xs: Seq[T], fun: (T, T) => T): T = {
    xs match {
      case Seq(x) => x
      case Seq(x, y) => fun(x, y)
      case _ => reduce(xs.grouped(2).map(reduce(_, fun)).toSeq, fun)
    }
  }

  implicit class TreeReducer[T](xs: Seq[T]) {
    def reduceTree(f: (T, T) => T): T = {
      xs match {
        case Seq(x) => x
        case Seq(x, y) => f(x, y)
        case _ => xs
        .grouped(2)
        .map(_.reduceTree(f))
        .toSeq
        .reduceTree(f)
      }
    }
  }




}
