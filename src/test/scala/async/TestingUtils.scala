package async

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util.log2Ceil
import chiseltest._
import chiseltest.internal.TesterThreadList

import scala.util.Random

object TestingUtils {

  implicit class SteppingExtension(c: Clock) {
    def stepUntil(p: => Boolean): Unit = while(!p) c.step()
  }

  implicit class PokeExpectExtension[T <: Bundle](x: T) {
    def poke(ps: T => (Data, Data)*): Unit = {
      ps.map(_(x)).foreach { case (field, value) => field.poke(value) }
    }
    def expect(es: T => (Data, Data)*): Unit = {
      es.map(_(x)).foreach { case (field, expected) => field.expect(expected) }
    }
  }

  implicit class ForkExtension(f: TesterThreadList) {
    def forkForEach[T](xs: Seq[T], region: Region = TestdriverMain)(fun: T => Any): TesterThreadList = {
      xs.foldLeft(f) { case (forkBuilder, item) => forkBuilder.fork.withRegion(region)(fun(item)) }
    }
  }
  object forkForEach {
    def apply[T](xs: Seq[T])(fun: T => Any): TesterThreadList = xs match {
      case head::tail => tail.foldLeft(fork(fun(head))) { case (forkBuilder, item) => forkBuilder.fork(fun(item)) }
    }
  }

}
