
import chisel3.internal.firrtl.Width
import chisel3.util.log2Ceil
import chisel3._
import firrtl.AnnotationSeq

import scala.language.implicitConversions
import scala.util.Random

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

    def splitInHalf: (Seq[T], Seq[T]) = {
      val (lower, upper) = xs.zipWithIndex.span(_._2 <= xs.length / 2)
      lower.map(_._1) -> upper.map(_._1)
    }
    def reduceTree(f: (T, T) => T): T = {
      println(xs)
      xs match {
        case Seq() => throw new Exception("this should not happen")
        case Seq(x) => x
        case Seq(x, y) => f(x, y)
        case _ =>
          val (firstHalf, secondHalf) = xs.splitInHalf
          f(firstHalf.reduceTree(f), secondHalf.reduceTree(f))
      }
    }
  }




object OptionalIO {

  def apply[T <: Data](gen: T, cond: Boolean): Option[T] = if(cond) Some(gen) else None

}

  def pow2(x: Int): Int = math.pow(2, x).toInt

  implicit class RandomExtension(r: Random.type) {
    def nextInt(range: Range): Int = range.start + Random.nextInt(range.size)

    def nextPow2(range: Range): Int = {
      val max = log2Ceil(range.end)
      val min = log2Ceil(range.start)
      pow2(nextInt(min to max))
    }

    def choose[T](xs: Seq[T]): T = xs.apply(Random.nextInt(xs.length))

    def nextUInt(range: Range): UInt = nextInt(range).U

    def nextBool(): Bool = Random.nextBoolean().B

    def nextUInt(width: Width): UInt = {
      val max = pow2(width.get.toInt)
      nextUInt(0 until max)
    }

  }


}
