package async

import async.TestingUtils.forkForEach
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
    def forkForEach2d[T](xs: Seq[Seq[T]], region: Region = TestdriverMain)(fun: T => Any): TesterThreadList = {
      xs.tail.foldLeft(f.forkForEach(xs.head)(fun)) { case (builder, list) => builder.forkForEach(list)(fun) }
    }
  }
  object forkForEach {
    def apply[T](xs: Seq[T])(fun: T => Any): TesterThreadList =
      xs.tail.foldLeft(fork(fun(xs.head))) { case (forkBuilder, item) => forkBuilder.fork(fun(item)) }
  }

  object forkForEach2d {
    def apply[T](xs: Seq[Seq[T]])(fun: T => Any): TesterThreadList = {
      xs.tail.foldLeft(forkForEach(xs.head)(fun)) { case (builder, list) => builder.forkForEach(list)(fun) }
    }
  }

}
