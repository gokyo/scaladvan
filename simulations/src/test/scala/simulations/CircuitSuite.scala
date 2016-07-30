package simulations

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "and 3")
    
    in1.setSignal(false)
    run
    assert(out.getSignal === false, "and 4")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")

    in1.setSignal(false)
    run
    assert(out.getSignal === false, "or 3")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 4")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")

    in1.setSignal(false)
    run
    assert(out.getSignal === false, "or 3")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 4")
  }

  test("demux 101") {
    val in = new Wire
    val o0 = new Wire
    demux(in, List(), List(o0))
    run
    assert(o0.getSignal === false, "demux 101 1")

    in.setSignal(true)
    run
    assert(o0.getSignal === true, "demux 101 2")

    in.setSignal(false)
    run
    assert(o0.getSignal === false, "demux 101 3")
  }
  
  test("demux 112") {
    val in = new Wire
    val c0 = new Wire
    val o0, o1 = new Wire
    demux(in, List(c0), List(o1, o0))
    run
    assert(o0.getSignal === false, "demux 112 1")
    assert(o1.getSignal === false, "demux 112 2")
    
    in.setSignal(true)
    run
    assert(o1.getSignal === false, "demux 112 3")
    assert(o0.getSignal === true, "demux 112 4")

    c0.setSignal(true)
    run
    assert(o1.getSignal === true, "demux 112 5")
    assert(o0.getSignal === false, "demux 112 6")
    
    in.setSignal(false)
    run
    assert(o1.getSignal === false, "demux 112 7")
    assert(o0.getSignal === false, "demux 112 8")
  }
  

  test("demux 124") {
    val in = new Wire
    val c0, c1 = new Wire
    val o0, o1, o2, o3 = new Wire
    demux(in, List(c1,c0), List(o3,o2,o1,o0))
    run
    assert(o0.getSignal === false, "demux 124 1")
    assert(o1.getSignal === false, "demux 124 2")
    assert(o2.getSignal === false, "demux 124 3")
    assert(o3.getSignal === false, "demux 124 4")
    
    in.setSignal(true)
    run
    assert(o0.getSignal === true, "demux 124 5")
    assert(o1.getSignal === false, "demux 124 6")
    assert(o2.getSignal === false, "demux 124 7")
    assert(o3.getSignal === false, "demux 124 8")

    c0.setSignal(true)
    run
    assert(o0.getSignal === false, "demux 124 9")
    assert(o1.getSignal === true, "demux 124 10")
    assert(o2.getSignal === false, "demux 124 11")
    assert(o3.getSignal === false, "demux 124 12")
    
    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(o0.getSignal === false, "demux 124 13")
    assert(o1.getSignal === false, "demux 124 14")
    assert(o2.getSignal === true, "demux 124 15")
    assert(o3.getSignal === false, "demux 124 16")
    
    c0.setSignal(true)
    run
    assert(o0.getSignal === false, "demux 124 17")
    assert(o1.getSignal === false, "demux 124 18")
    assert(o2.getSignal === false, "demux 124 19")
    assert(o3.getSignal === true, "demux 124 20")
    
    in.setSignal(false)
    run
    assert(o0.getSignal === false, "demux 124 21")
    assert(o1.getSignal === false, "demux 124 22")
    assert(o2.getSignal === false, "demux 124 23")
    assert(o3.getSignal === false, "demux 124 24")
  }
  
}