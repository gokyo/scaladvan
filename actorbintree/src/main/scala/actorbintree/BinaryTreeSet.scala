package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request to perform garbage collection
   */
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /**
   * Message to signal successful completion of an insert or remove operation. 
   */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor
  with ActorLogging {
  
  import BinaryTreeSet._
  import BinaryTreeNode._

  val rootProps = BinaryTreeNode.props(0, true)
  
  def createRoot: ActorRef = {
    rootCounter += 1
    context.actorOf(rootProps, "root-" + (rootCounter))
  }
  
  var rootCounter: Int = -1
  
  var root: ActorRef = createRoot

  var pendingQueue: Queue[Operation] = Queue.empty[Operation]

  // Behaviours
  
  /**
   * Accepts `Operation` and `GC` messages. 
   */
  val normalBehaviour: Receive = {
    
    case Insert(req: ActorRef, id: Int, x: Int) => 
      root ! Insert(req, id, x)
    
    case Contains(req: ActorRef, id: Int, x: Int) => 
      root ! Contains(req, id, x)
    
    case Remove(req: ActorRef, id: Int, x: Int) => 
      root ! Remove(req, id, x)
    
    case GC =>
      // I copy the tree nodes into a new root
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      
      // Once I receive 'GC', I replace the actor behaviour (type = Receive)
      // to the one inside the 'garbageCollecting' method.
      // Since, the boolean 'discardOld' parameter is not passed, we simply replace the 
      // actor behaviour on the top of the stack, with the new behaviour. 
      context become garbageCollectingBehaviour(newRoot)
  }

  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollectingBehaviour(newRoot: ActorRef): Receive = {
    
    case Insert(req: ActorRef, id: Int, x: Int) =>  
      pendingQueue :+= Insert(req, id, x)
    
    case Contains(req: ActorRef, id: Int, x: Int) =>  
      pendingQueue :+= Contains(req, id, x)
    
    case Remove(req: ActorRef, id: Int, x: Int) =>  
      pendingQueue :+= Remove(req, id, x)
    
    case CopyFinished =>
      // I stop the 'root' actor, sending him a PoisonPill
      // All children actors of the root (namely 
      // all root children nodes in the tree) will be stopped
      root ! PoisonPill
      // I switch the root to the new one
      root = newRoot
      pendingQueue foreach { newRoot ! (_: Operation) }
      // I clear the queue of the operations
      pendingQueue = Queue.empty[Operation]
      
      // the current behaviour in the stack head is back to 'normal'
      context.become(normalBehaviour)
    
    case GC => // ignore

  }
  
  /**
   * The initial behaviour is the normal one
   */
  val receive: Receive = normalBehaviour

}
