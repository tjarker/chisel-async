package helpers

import chisel3._

object Hardware {

  object SetReg {
    def apply(event: Bool): Bool = {
      val setReg = RegInit(0.B)
      when(event) {
        setReg := 1.B
      }
      setReg
    }
  }

  object SetRegWithValue {
    def apply(value: Bool, event: Bool): Bool = {
      val setReg = RegInit(0.B)
      when(event) {
        setReg := value
      }
      setReg
    }
  }

  object ToggleReg {
    def init(init: Bool, event: Bool = 1.B): Bool = {
      val toggleReg = RegInit(init)
      when(event) {
        toggleReg := !toggleReg
      }
      toggleReg
    }
  }

  def bruteForceSynchronize[T <: Data](x: T): T = RegNext(RegNext(x, 0.U.asTypeOf(x)), 0.U.asTypeOf(x))

  def risingEdge(x: Bool) = x && !RegNext(x, 0.B)



}
