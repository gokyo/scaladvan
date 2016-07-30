import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
import scala.concurrent.Promise

/**
 * Contains basic data types, data structures and `Future` extensions.
 */
package object nodescala {

  /**
   * Adds extensions methods to the `Future` companion object.
   */
  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {

    /**
     * Returns a future that is always completed with `value`.
     */
    def always[T](value: T): Future[T] = {
      // Two quick ways to implement this method:
      // ========================================
      //  1.   Future.successful(value)
      //  2.   Promise.successful(value).future

      val p = Promise[T]
      //p.complete(Try(value))
      p.success(value)
      p.future
    }

    /**
     * Returns a future that is never completed.
     *
     *  This future may be useful when testing if timeout logic works correctly.
     */
    def never[T]: Future[T] = {
      Promise[T].future
    }

    /**
     * Given a list of futures `fs`, returns the future holding the list of values of all the futures from `fs`.
     *  The returned future is completed only once all of the futures in `fs` have been completed.
     *  The values in the list are in the same order as corresponding futures `fs`.
     *  If any of the futures `fs` fails, the resulting future also fails.
     */
    def all[T](fs: List[Future[T]]): Future[List[T]] = {

      // Easiest implementation
      //Future.sequence(fs) 

      //Implemented using 'foldRight', as seen in the last lecture of week 3
      //The 'seed' (zero element) for the fold is: Future.always(List[T]())
      fs.foldRight[Future[List[T]]](Future.always(List[T]())) {
        (fut: Future[T], acc: Future[List[T]]) =>
          for {
            result <- fut
            fi: List[T] <- acc
          } yield (result :: fi): List[T]
      }

      //Implemented using 'foldLeft'. But not very efficient I guess
//      fs.foldLeft(Future.always(List[T]())) {
//        (acc, fut) =>
//          for {
//            fi <- acc
//            res <- fut
//          } yield (fi ::: (List(res)))
//      }
  
    }
    
    /**
     * Given a list of futures `fs`, returns the future holding the value of the future from `fs` that completed first.
     *  If the first completing future in `fs` fails, then the result is failed as well.
     *
     *  E.g.:
     *
     *      Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))
     *
     *  may return a `Future` succeeded with `1`, `2` or failed with an `Exception`.
     */
    def any[T](fs: List[Future[T]]): Future[T] = {
            
      val p = Promise[T]

      // 1st implementation:
      // ===================
      // Each future of the list 'fs', once it is completed,  
      // will try to complete the promise 'p' with his return value. 
      // Since a promise is completed only once, 
      // only the future that finishes first will complete 'p' 
      fs foreach {
        (ff: Future[T]) =>
          ff onComplete {
            res: Try[T] =>
              if (!p.isCompleted) {
                p.tryComplete(res)
              }
          }
      }

//      // 2nd implementation: using 'map'
//      // ===============================
//      val p = Promise[T]
//      fs.map(ff => ff onComplete {
//        res: Try[T] =>
//          if (!p.isCompleted) {
//            p.tryComplete(res)
//          }
//      })
      
      p.future
      
    }

    /**
     * Returns a future with a unit value that is completed after time `t`.
     */
    def delay(t: Duration): Future[Unit] = {
      // creates and starts an asynchronous computation
      Future {
        blocking {
          // sleeps, blocking the current thread
          Thread.sleep(t.toMillis)
        }
        (): Unit // returns the result value, the unit value ()
      }
    }

    /**
     * Completes this future with user input.
     */
    def userInput(message: String): Future[String] = Future {
      // Bug in the provided code: 
      // - 'blocking' added in order to solve the issue
      // 
      // https://class.coursera.org/reactive-001/forum/thread?thread_id=888
      
      blocking {
        readLine(message)
      }
    }

    /**
     * Creates a cancellable context for an execution and runs it.
     */
    def run()(f: CancellationToken => Future[Unit]): Subscription = {
      val token = CancellationTokenSource()
      f(token.cancellationToken) 
      token
    }
    
  }

  /**
   * Adds extension methods to future objects.
   */
  implicit class FutureOps[T](val f: Future[T]) extends AnyVal {

    /**
     * Returns the result of this future if it is completed now.
     *  Otherwise, throws a `NoSuchElementException`.
     *
     *  Note: This method does not wait for the result.
     *  It is thus non-blocking.
     *  However, it is also non-deterministic -- it may throw or return a value
     *  depending on the current state of the `Future`.
     */
    def now: T = {
      // implemented as explained here:
      // https://class.coursera.org/reactive-001/forum/thread?thread_id=852#post-3350
      try {
        Await.result(f, 0 nanos);
      } catch {
        case e: Throwable => 
          throw new NoSuchElementException(e.getMessage)
      }
    }

    /**
     * Continues the computation of this future by taking the current future
     *  and mapping it into another future.
     *
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continueWith[S](cont: Future[T] => S): Future[S] = {
      val p: Promise[S] = Promise[S]
      this.f onComplete {
        _: Try[T] => p complete Try(cont(f))
      }
      p.future
    }

    /**
     * Continues the computation of this future by taking the result
     *  of the current future and mapping it into another future.
     *
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continue[S](cont: Try[T] => S): Future[S] = {
      val p: Promise[S] = Promise[S]
      this.f onComplete {
        result: Try[T] => p.complete(Try(cont(result)))
      }
      p.future
    }

  }
  
  /**
   * Subscription objects are used to be able to unsubscribe
   *  from some event source.
   */
  trait Subscription {
    def unsubscribe(): Unit
  }

  object Subscription {
    
    /**
     * Given two subscriptions `s1` and `s2` returns a new composite subscription
     *  such that when the new composite subscription cancels both `s1` and `s2`
     *  when `unsubscribe` is called.
     */
    def apply(s1: Subscription, s2: Subscription) = new Subscription {
      def unsubscribe() {
        s1.unsubscribe()
        s2.unsubscribe()
      }
    }
  }

  /**
   * Used to check if cancellation was requested.
   */
  trait CancellationToken {
    def isCancelled: Boolean
    def nonCancelled = !isCancelled
  }

  /**
   * The `CancellationTokenSource` is a special kind of `Subscription` that
   *  returns a `cancellationToken` which is cancelled by calling `unsubscribe`.
   *
   *  After calling `unsubscribe` once, the associated `cancellationToken` will
   *  forever remain cancelled -- its `isCancelled` will return `false.
   */
  trait CancellationTokenSource extends Subscription {
    def cancellationToken: CancellationToken
  }

  /**
   * Creates cancellation token sources.
   */
  object CancellationTokenSource {
    
    /**
     * Creates a new `CancellationTokenSource`.
     */
    def apply(): CancellationTokenSource = new CancellationTokenSource {
      
      val p = Promise[Unit]
      
      val cancellationToken = new CancellationToken {
        def isCancelled = p.future.value != None
        //def isCancelled = p.isCompleted
      }
      
      def unsubscribe(): Unit = {
        if (!p.isCompleted) {
          p.trySuccess(())
        }
      }
    }
    
  }
  
}
