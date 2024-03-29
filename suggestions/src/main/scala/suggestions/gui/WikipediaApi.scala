package suggestions
package gui

import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.{ Observer, Observable }
import rx.lang.scala.Notification
import rx.lang.scala.Notification.{ OnError, OnNext, OnCompleted }
import rx.lang.scala.subjects.{ PublishSubject }
import observablex._
import search._

trait WikipediaApi {

  /**
   * Returns a `Future` with a list of possible completions for a search `term`.
   */
  def wikipediaSuggestion(term: String): Future[List[String]]

  /**
   * Returns a `Future` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikipediaPage(term: String): Future[String]

  /**
   * Returns an `Observable` with a list of possible completions for a search `term`.
   */
  def wikiSuggestResponseStream(term: String): Observable[List[String]] = ObservableEx(wikipediaSuggestion(term))

  /**
   * Returns an `Observable` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikiPageResponseStream(term: String): Observable[String] = ObservableEx(wikipediaPage(term))

  implicit class StringObservableOps(obs: Observable[String]) {

    /**
     * Given a stream of search terms, returns a stream of search terms with spaces replaced by underscores.
     *
     * E.g.
     * `"erik", "erik meijer", "martin`
     * should become
     * `"erik", "erik_meijer", "martin"`
     */
    def sanitized: Observable[String] =
      
      //obs.map { (s: String) => s.replaceAll(" ", "_") }

      obs map { s: String =>
        s map {
          case ' ' => '_'
          case c: Char => c
        }
      }
    
  }

  implicit class ObservableOps[T](obs: Observable[T]) {

    /**
     * Given an observable that can possibly be completed with an error, returns a new observable
     * with the same values wrapped into `Success` and the potential error wrapped into `Failure`.
     *
     * E.g.
     * `1, 2, 3, !Exception!`
     * should become
     * `Success(1), Success(2), Success(3), Failure(Exception), !TerminateStream!`
     */
    def recovered: Observable[Try[T]] = {

	  // We apply first the 'map' combinator, wrapping every value 
      // of the observable 'obs' inside 'Success' (it works also using 'Try').
      // Of course when we have an exception, the new observable throws it.
      // We could pattern-match the exception inside the 'map', but we didn't  
      // because we also want to complete the observable after the 1st exception is found.
      // In order to achieve this, we then apply the 'onErrorReturn' combinator,
      // which replace the exception t with another value (Failure(t) in our case), 
      // terminating the stream after.
      
      obs map { Success(_) } onErrorReturn { Failure(_) }

      // Other implementations:
      // ----------------------
      
      // obs map Success.apply onErrorReturn Failure.apply
      
//      obs.map {
//        case (t: T) => Try(t)
//      }.onErrorReturn {
//        case (e: Throwable) => Failure(e)
//      }
      
      
    }

    /**
     * Emits the events from the `obs` observable, until `totalSec` seconds have elapsed.
     *
     * After `totalSec` seconds, if `obs` is not yet completed, the result observable becomes completed.
     *
     * Note: uses the existing combinators on observables.
     */
    def timedOut(totalSec: Long): Observable[T] =
      obs.takeUntil(Observable.interval(totalSec seconds))
      
    /**
     * Given a stream of events `obs` and a method `requestMethod` to map a request `T` into
     * a stream of responses `S`, returns a stream of all the responses wrapped into a `Try`.
     * The elements of the response stream should reflect the order of their corresponding events in `obs`.
     *
     * E.g. given a request stream:
     *
     * 1, 2, 3, 4, 5
     *
     * And a request method:
     *
     * num => if (num != 4) Observable.just(num) else Observable.error(new Exception)
     *
     * We should, for example, get:
     *
     * Success(1), Success(2), Success(3), Failure(new Exception), Success(5)
     *
     *
     * Similarly:
     *
     * Observable(1, 2, 3).concatRecovered(num => Observable(num, num, num))
     *
     * should return:
     *
     * Observable(Success(1), Success(1), Success(1),
     * 			  Success(2), Success(2), Success(2),
     * 			  Success(3), Success(3), Success(3))
     */
    def concatRecovered[S](requestMethod: T => Observable[S]): Observable[Try[S]] = {

      obs flatMap { requestMethod(_).recovered }

      // Below other possible implementations, using these properties:
      //
      // 1.	  map(f).flatten = flatMap(f)
      // 2.   Observable.flatten and Observable.concat produce the same result
      //
      // ----------------------
      //
      // obs.map(requestMethod(_).recovered).flatten
      // obs.map(requestMethod(_).recovered).concat
      
    }

  }
}
