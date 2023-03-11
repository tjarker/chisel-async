package async

import chisel3._
import chiseltest._

object HandshakeTesting {



  implicit class HandshakeDriver[T <: Data](x: Handshake[T]) {

    def initSource(tick: Clock): this.type = {
      ClockResolutionUtils.setClock(HandshakeDriver.handshakeSourceKey, x, tick)
      x.req.poke(false.B)
      this
    }

    protected def getSourceClock: Clock = {
      ClockResolutionUtils.getClock(
        HandshakeDriver.handshakeSourceKey,
        x,
        x.ack.getSourceClock()
      )
    }

    def send(data: T): Unit = {
      x.data.poke(data)
      x.req.poke((!x.req.peekBoolean()).B)
      val old = x.ack.peekBoolean()
      while (old == x.ack.peekBoolean()) {
        getSourceClock.step(1)
      }
      getSourceClock.step(1)
    }

    def send(data: T, datas: T*): Unit = send(data +: datas)

    def send(data: Seq[T]): Unit = {
      for (elt <- data) {
        send(elt)
      }
    }

    // Sink (dequeue) functions
    //
    def initSink(tick: Clock): this.type = {
      ClockResolutionUtils.setClock(HandshakeDriver.handshakeSinkKey, x, tick)
      x.ack.poke(false.B)
      this
    }

    protected def getSinkClock: Clock = {
      ClockResolutionUtils.getClock(
        HandshakeDriver.handshakeSinkKey,
        x,
        x.req.getSourceClock()
      )
    }

    def waitForToken(): Unit = {
      val old = x.req.peekBoolean()
      getSinkClock.step(1)
      while (old == x.req.peekBoolean()) {
        getSinkClock.step(1)
      }
    }

    def receive(): T = {
      waitForToken()
      val payload = x.data.peek()
      getSinkClock.step(1)
      x.ack.poke((!x.ack.peekBoolean()).B)
      getSinkClock.step(1)
      payload
    }

    def receiveSeq(n: Int): Seq[T] = {
      Seq.fill(n)(receive())
    }

    def receiveExpect(data: T): Unit = {
        waitForToken()
        x.data.expect(data)
        getSinkClock.step(1)
        x.ack.poke((!x.ack.peekBoolean()).B)
    }

    def receiveExpect(data: Seq[T]): Unit = {
      for(elem <- data) {
        receiveExpect(elem)
      }
    }

    def receiveExpect(data: T, datas: T*): Unit = receiveExpect(data +: datas)

  }

  object HandshakeDriver {
    protected val handshakeSourceKey = new Object()
    protected val handshakeSinkKey = new Object()
  }

}
