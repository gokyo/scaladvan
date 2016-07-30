package suggestions
package observablex

import scala.concurrent.{ Future, ExecutionContext }
import scala.util._
import scala.util.Success
import scala.util.Failure
import java.lang.Throwable
import rx.lang.scala.Observable
import rx.lang.scala.Scheduler
import rx.lang.scala.subjects.ReplaySubject

object ObservableEx {

  /**
   * Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {

    // Taken from the following link: 
    // https://github.com/headinthebox/CourseraCodeSamplesReactiveProgramming/blob/master/src/test/scala/coursera/rx/ToObservable.scala
    // But using ReplaySubject as requested in the assignment
    
    val subject = ReplaySubject[T]

    // when the future f completes, ... 
    f onComplete {
      
      case Failure(e: Throwable) =>
        // Propagates the error to the Observable
        subject onError e
        
      case Success(c) =>
        // Propagates the value 'c' to the Observable
        subject onNext c 
        // The future has completed with the value 'c'
        // Thus, we can complete also the subject 
        // (that has just the single value 'c' in its stream) 
        subject.onCompleted()
    }

    subject
  }

}