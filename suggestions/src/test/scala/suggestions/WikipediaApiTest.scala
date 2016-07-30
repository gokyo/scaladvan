package suggestions

import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import rx.lang.scala.concurrency.Schedulers
import org.scalatest._
import gui._
import suggestions.observablex.ObservableEx


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for { suffix <- List(" (Computer Scientist)", " (Footballer)") }
          yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
  
  // =========
  // NEW TESTS
  // =========
  
  test("ObservableEx#apply() on success") {
    val value: Int = 66; 
    val future: Future[Int] = Future(value)
    val observable: Observable[Int] = ObservableEx(future)
    val observableResult: List[Int] = observable.toBlockingObservable.toList
    
    assert(observableResult.size === 1)
    assert(observableResult === List(value))
    
    assert(future.value === Some(Success(value)))
    assert(future.isCompleted)
  }
  
  test("ObservableEx#apply() on failure") {
    val future: Future[Nothing] = Future.failed(new Exception("KO"))
    val observable: Observable[Nothing] = ObservableEx(future)
    intercept[Exception] {
      observable.toBlockingObservable.toList
    }  
  }

  test("recovered 1") {
    val notvalid: Observable[Int] =
      Observable(1, 2, 3) ++ Observable(new Exception("E")) ++ Observable(4, 5)
    val valid = notvalid.recovered

    var tries = 0
    var exceptions = 0
    var completed = false

    val sub = valid.subscribe(
      observer => {
        try {
          observer.get
          tries += 1
        } catch {
          case e: Exception => exceptions += 1
        }
      },
      t => assert(false, s"stream error $t"),
      () => completed = true)

    val expected = Observable(Try(1), Try(2), Try(3), Try(new Exception("E")))
    assert(completed, "completed")
    assert(tries === 3, "tries")
    assert(exceptions === 1, "exceptions")
  }

  test("recovered 2") {
    val requests = Observable(3, 2, 1, 5, 7)
    val comp = requests.map(i => i / (i - 1))

    val theList = comp.recovered.map(_.isFailure).toBlockingObservable.toList
    assert(theList === List(false, false, true))
  }

  test("timedOut") {
    // 'clock' observable has a new value every 33 millis    
    val clock = Observable.interval(33 millis)

    // 'timedOut' stops after 1 sec = 1000 millis    
    val timedOut = clock.timedOut(1)

    // list of 30 numbers. 
    // In fact:
    //  - 30 * 33 =  990 < 1000 
    //  - 31 * 33 = 1023 > 1000  
    val results: List[Long] = timedOut.toBlockingObservable.toList

    assert(results.length === 30)
  }

  test("concatRecovered 1") {
    val req: Observable[Int] = Observable(1, 2, 3, 4, 5)
    val response: Observable[Try[Int]] = req.concatRecovered {
      (num: Int) =>
        if (num != 3) Observable(num)
        else Observable(new Exception)
    }

    val res = response.foldLeft((0, 0)) { (acc, tn) =>
      (tn: Try[Int]) match {
        case Success(n) => (acc._1 + n, acc._2)
        case Failure(_) => (acc._1, acc._2 + 1)
      }
    }

    var pair = (0, 0)
    res.observeOn(Schedulers.immediate).subscribe(e => pair = e)
    val (sum, fc) = pair
    assert(sum == (1 + 2 + 4 + 5))
    assert(fc == 1)
  }

  test("concatRecovered 2") {
    val exception = new Exception()
    val expected = List(
      Success(2), Success(1), Failure(exception), Success(1), Success(0), Failure(exception))

    val actual = Observable(2, 1).concatRecovered {
      i => Observable(i, i - 1) ++ Observable(exception)
    }.toBlockingObservable.toList
    assert(actual === expected)
  }

  test("concatRecovered 3") {
    val expected = List(
      Success(1), Success(1), Success(1), Success(1),
      Success(2), Success(2), Success(2), Success(2),
      Success(3), Success(3), Success(3), Success(3))

    val actual = Observable(1, 2, 3).concatRecovered {
      (num: Int) => Observable(num, num, num, num)
    }.toBlockingObservable.toList

    assert(actual === expected)
  }
  
}