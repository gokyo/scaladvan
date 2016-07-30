package nodescala

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.concurrent.TimeoutException

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  val ERR_MSG = "TEST ERROR"
  
  val exc: Exception = new IndexOutOfBoundsException(ERR_MSG)
    
  test("Future initial tests") {
    // here the exception is not thrown
    val f2 = Future(throw exc)
    // if I use Await#result here, the exception of the future is thrown
    // Await#ready is used instead.
    Await.ready(f2, 50 milliseconds)
    assert(f2.value.get.isFailure)
    assert(f2.value === Some(Failure(exc)))
    assert(f2.isCompleted)

    // here the exception is not thrown
    val f3 = Future.failed(exc)
    Await.ready(f3, 50 milliseconds)
    assert(f3.value.get.isFailure)
    assert(f3.value === Some(Failure(exc)))
    assert(f3.isCompleted)
    
    // here the exception is thrown
    try {
      Future.successful(throw exc)
      fail // not reached
    } catch {
      case t: IndexOutOfBoundsException => // ok!
      case e: Exception => fail // not reached
    }
  }
    
  test("always") {
    val number: Int = 56
	val always = Future.always(number)
	// Await#result returns the value of the (successful) future 
    assert(Await.result(always, 0 nanos) == number)
    assert(always.value === Some(Success(number)))
    assert(always.isCompleted)
    
    val f1 = Future.always(exc)
    assert(Await.result(f1, 0 nanos) === exc)
    assert(f1.value.get.isSuccess)
    assert(f1.value === Some(Success(exc)))
    assert(f1.isCompleted)
    
    // here the exception is thrown
    try {
      Future.always(throw exc)
      fail // not reached
    } catch {
      case t: IndexOutOfBoundsException => // ok!
      case e: Exception => fail // not reached
    }
  }

  test("never") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      fail // not reached
    } catch {
      case t: TimeoutException => // ok!
      case e: Exception => fail // not reached
    }
    
    assert(!never.isCompleted)
  }
  
  test("any") {
    val anyOfAlways = Future.any(List(Future.never[Int], Future.always(18)))
    assert(Await.result(anyOfAlways, 50 milliseconds) === 18)

    // Futures f1, f2 are created and they start their computation asynchronously
    val f1: Future[String] = Future[String] {
      blocking {
        Thread.sleep(80)
      }
      "1st" // result value
    }
    val f2: Future[String] = Future {
      blocking {
        Thread.sleep(160)
      }
      "2nd" // result value
    }
    
    val futureTooEarly: Future[String] = Future.any(List(f1, f2))
    try {
      val x: String = Await.result(futureTooEarly, 1 nano)
      fail // not reached
    } catch {
      case t: TimeoutException => // ok!
      case e: Exception => fail // not reached
    }
    assert(!f1.isCompleted)
    assert(!f2.isCompleted)
    
    val anyFirst = Future.any(f1 :: f2 :: Nil)
    assert(Await.result(anyFirst, 90 milliseconds) === "1st")
    assert(f1.isCompleted)
    assert(!f2.isCompleted)
    Thread.sleep(100)
    assert(f2.isCompleted)
    
    val excFuture: Future[Exception] = Future { 
      throw exc 
    }
    
    // Note: 'future' is deprecated! You should use 'Future'
    val f3: Future[String] = future {
      blocking {
        Thread.sleep(999)
      }
      "3rd" // result value
    }
    
    val any3 = Future.any(List(excFuture, Future.never, f3))
    try {
      Await.result(any3, 10 nanos)
      assert(false) // not reached 
    } catch {
      case t: IndexOutOfBoundsException => 
        assert(t.getMessage() === ERR_MSG)
        assert(any3.value.get.isFailure)
      case e: Exception => fail // not reached
    }
    
    assert(excFuture.isCompleted)
    assert(!f3.isCompleted)
    assert(f2.isCompleted)
    
    val fs = Future.always(111) :: Future.always(222) :: Nil
    val any = Future.any(fs)
    val r = Await.result(any, 130 millis) 
    assert(r == 111 || r == 222)
  }
  
  test("all") {
    val numbers: List[Int] = (1 to 6).toList 
    val futures: List[Future[Int]] = numbers map { Future always _ }
    val futureAll: Future[List[Int]] = Future all futures
     
    val actual = Await.result(futureAll, 200 millis)
    assert(futureAll.isCompleted)
    assert(actual === numbers)

    intercept[IndexOutOfBoundsException] {
      //  futures order in the list matters: IndexOutOfBoundsException
      val fs = List(
        Future.always { 1 },
        Future { throw exc },
        Future.never)
      val fa = Future.all(fs)
      Await.result(fa, 1 second)
    }

    intercept[TimeoutException] {
      //  futures order in the list matters: TimeoutException
      val fs = List(
        Future.always { 1 },
        Future.never,
        Future { throw exc })
      val fa = Future.all(fs)
      Await.result(fa, 1 second)
    }
    
    val all2 = Future.all(List(
      Future.always(44),
      Future { throw exc }))
    val error = Await.ready(all2, 100 milliseconds)
    assert(all2.isCompleted)
    assert(error.value.get.isFailure)
  }
  
  test("delay") {
    val wait: FiniteDuration = 60 milliseconds

    val f = Future.delay(110 milliseconds)
    try {
      Await.ready(f, wait)
      assert(!f.isCompleted)
      fail // not reached
    } catch {
      case t: TimeoutException => // ok!
      case e: Exception => fail // not reached
    }

    val f1 = Future.delay(wait)
    val result = Await.result(f1, 80 milliseconds)
    assert(result === ())
    assert(f1.isCompleted)
  }

  test("now") {
    val value: Int = 50
    assert(Future.always(value).now === value)

    val ften = Future.always(10)
    val f = Await.result(ften, Duration.Inf)
    assert(f === 10)
    assert(f === ften.now)   
    
    try {
      Future.never.now
      fail // not reached
    } catch {
      case t: NoSuchElementException => // ok!
      case e: Exception => fail // not reached
    }
    
    try {
      Future.failed(throw exc).now
      fail // not reached
    } catch {
      case t: IndexOutOfBoundsException => // ok!
      case s: NoSuchElementException => fail // not reached
      case e: Exception => fail // not reached
    }
  }

  test("continue") {
    val f1: Future[Int] = Future{ 110 }
    val futCont1: Future[_] = f1 continue { _ => throw exc }
    intercept[IndexOutOfBoundsException] {
      Await.result(futCont1, 100 milli)
    }

    val msg = "Ciao"
    val f2: Future[_] = Future { throw exc }
    val futCont2: Future[String] = f2.continue( _ => msg )
    assert(msg === Await.result(futCont2, 100 milli))
  }

  test("continueWith") {
    val msg: String = "GutenMorgen"
    val f1: Future[_] = Future { throw exc }
    val futContinued: Future[String] = f1.continueWith { f => msg }
    assert(Await.result(futContinued, 100 millis) === msg)
  }
  
  test("run") {
    import org.scalatest.concurrent.AsyncAssertions._
    import org.scalatest.time._

    val w = new Waiter
    val p = Promise[Unit]()
    val working = Future.run() { ct =>
      async {
    	// 'async { ... }' is like 'Future { ... }'
        while (ct.nonCancelled) {
          Thread.sleep(100)
        }
        p.success() // I complete the promise
        w { 
          assert(true) 
        }
        w.dismiss()
      }
    }
    
    Future.delay(600 millis) onSuccess {
      case _ => working.unsubscribe()
    }
    
    w.await(Timeout(Span(2, Seconds)))
    assert(p.future.isCompleted)
  }

  test("CancellationTokenSource") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }
      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) === "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}
