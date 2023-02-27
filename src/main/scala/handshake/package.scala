import chisel3.{Bool, Bundle, Data, Input, Output}

package object handshake {

  sealed abstract class HandshakeInitializer[+T] {
    def isValid: Boolean

    def getInitOption: Option[T]
  }

  object HandshakeInitializer {
    case object Empty extends HandshakeInitializer[Nothing] {
      def isValid = false

      def getInitOption = None
    }

    final case class ValidToken[T <: Data](value: T) extends HandshakeInitializer[T] {
      def isValid = true

      def getInitOption = Some(value)
    }
  }

  class Handshake[T <: Data](gen: T) extends Bundle {
    val req = Output(Bool())
    val ack = Input(Bool())
    val payload = Output(gen)
  }

  object Handshake {
    def apply[T <: Data](gen: T) = new Handshake(gen)
  }

}
