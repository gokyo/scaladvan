package simulations

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { 
        output.setSignal(!inputSig) 
      }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { 
        output.setSignal(a1Sig & a2Sig) 
      }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { 
        output.setSignal(a1Sig | a2Sig) 
      }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(in1: Wire, in2: Wire, output: Wire) {
    val notIn1, notIn2, notOut = new Wire
    inverter(in1, notIn1)
    inverter(in2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }

  /**
   * I needed some help from here:
   * 
   * https://class.coursera.org/reactive-001/forum/thread?thread_id=348#post-1837
   * https://coursera-forum-screenshots.s3.amazonaws.com/4e/bb0becad4b1d6cd9c331c5c3b3bf3d/Demux.png
   * https://www.evernote.com/shard/s26/sh/56343151-62e2-43e7-bc01-59af771675b4/9adff03cb7a044c4cd1816a4ed260c22
   * 
   * http://www.allaboutcircuits.com/vol_4/chpt_9/4.html
   * http://www.allaboutcircuits.com/vol_4/chpt_9/6.html
   */
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    // in: input signal
    // c: list of n control wires, where the first one is at the end of the list
    // out: list of 2^n output wires, where the first one is at the end of the list

    def addDemuxAction(input: Wire, output: Wire) {
      // since delay = 0, we are using an anonymous function 
      input addAction {
        () => output.setSignal(input.getSignal)
      }
    }

    def addDemuxActions(input: List[Wire], output: List[Wire]) {
      input.zip(output).foreach(wires => addDemuxAction(wires._1, wires._2))
    }

    def getDemuxWires(input: Wire, controls: List[Wire]): List[Wire] = {
      controls match {
        case cc :: cs =>
          val notC1, demuxOut1, demuxOut2 = new Wire
          inverter(cc, notC1)
          andGate(notC1, input, demuxOut1)
          andGate(cc, input, demuxOut2)
          getDemuxWires(demuxOut2, cs) ::: getDemuxWires(demuxOut1, cs) 
        case Nil =>
          List(input)
      }
    }

    addDemuxActions(getDemuxWires(in, c), out)
  }
}

object Circuit extends CircuitSimulator {
  
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)

    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in1.setSignal(false)
    run
    
    in2.setSignal(true)
    run
  }

  def orGate2Example {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)

    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in1.setSignal(false)
    run
    
    in2.setSignal(true)
    run
  }
  
  def demux101Example {
    val in, out = new Wire
    demux(in, Nil, List(out))
    probe("in", in)
    probe("out", out)

    in.setSignal(false)
    run

    in.setSignal(true)
    run
  }

  def demux112Example {
    // TODO
  }

  def demux124Example {
    // TODO
  }
  
}

object CircuitMain extends App {
  
  println
  println("AND GATE EXAMPLE")
  Circuit.andGateExample
  
  println
  println("OR GATE EXAMPLE")
  Circuit.orGateExample
  
  println
  println("OR GATE 2 EXAMPLE")
  Circuit.orGate2Example

  println
  println("DEMUX 101 EXAMPLE")
  Circuit.demux101Example
  
  // TODO: add other demux examples
  
}