import async.primitives.DelayElement
import chisel3._

package object async {

  case class DelayElementConfig(simulation: Boolean, delayElementFactory: () => DelayElement)

  sealed abstract class HandshakeInitializer[+T] {
    def hasToken: Boolean

    def tokenOption: Option[T]
  }

  case object Empty extends HandshakeInitializer[Nothing] {
    def hasToken = false

    def tokenOption = None
  }

  final case class Token[T <: Data](value: T) extends HandshakeInitializer[T] {
    def hasToken = true

    def tokenOption = Some(value)
  }

  class Handshake[T <: Data](gen: T) extends Bundle {
    val req = Output(Bool())
    val ack = Input(Bool())
    val data = Output(gen)
  }

  object HandshakeOut {
    def apply[T <: Data](gen: T) = new Handshake(gen)
  }

  object HandshakeIn {
    def apply[T <: Data](gen: T) = Flipped(new Handshake(gen))
  }

}
