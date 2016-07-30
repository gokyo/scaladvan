package nodescala

import scala.language.postfixOps
import scala.concurrent._
import scala.util._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{ async, await }

object Main {

  val END = "Bye!"
  
  def getMessageAfterDelay(d: Duration, msg: String): Future[String] = {
    val p = Promise[String]
    Future.delay(d) onComplete {
      case (_: Try[Unit]) => p.success(msg)
    }
    p.future
  }
  
  def main(args: Array[String]) {
    
    // 1. instantiate the server at 8191, relative path "/test",
    //    and have the response return headers of the request
    val myServer = new NodeScala.Default(8191)
    
    val myServerSubscription = myServer.createListener("/test").start

    // 2. create a future that expects some user input `x`
    //    and continues with a `"You entered... " + x` message
    val userInterrupted: Future[String] =
      Future.userInput("Enter a message here: ").continue {
        (msg: Try[String]) => "You entered ..." + msg.get
      }

    // 3. create a future that completes after 20 seconds
    //    and continues with a `"Server timeout!"` message
    val timeOut: Future[String] =
      getMessageAfterDelay(20 seconds, "Server timeout!")

    // 4. create a future that completes when either 20 seconds elapse
    //    or the user enters some text and presses ENTER
    val terminationRequested: Future[String] =
      Future.any[String](userInterrupted :: timeOut :: Nil)
      
    // 5. unsubscribe from the server
    terminationRequested onSuccess {
      case msg: String =>
        println
        println
        println(msg)
        myServerSubscription.unsubscribe
        println
        println(END)
    }
    
  }

}