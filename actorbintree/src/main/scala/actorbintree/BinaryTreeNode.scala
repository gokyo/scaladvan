package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import akka.actor.ActorContext

object BinaryTreeNode {
  
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean): Props =
    Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor
  with ActorLogging {
 
  import BinaryTreeNode._
  import BinaryTreeSet._

  var removed = initiallyRemoved
  
  var subtrees = Map[Position, ActorRef]()

  // Behaviours
  
  /**
   * Handles `Operation` messages and `CopyTo` requests.
   */
  val normalBehaviour: Receive = {
    
    case Insert(req: ActorRef, id: Int, x: Int) =>
      performOperation(Insert(req, id, x), InsertJob)
    
    case Contains(req: ActorRef, id: Int, x: Int) =>
      performOperation(Contains(req, id, x), ContainsJob)
    
    case Remove(req: ActorRef, id: Int, x: Int) =>
      performOperation(Remove(req, id, x), RemoveJob)
    
    case CopyTo(node) => 
      if (removed && subtrees.isEmpty) {
        sender ! CopyFinished
      } else {
        if (!removed) {
          node ! Insert(self, id = elem, elem)
        }
        val childrenNodes: Set[ActorRef] = subtrees.values.toSet
        childrenNodes foreach { _ ! CopyTo(node) }
        context become copyingBehaviour(sender, childrenNodes, removed) 
      }

  }
  
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copyingBehaviour(requester: ActorRef, expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    
    case CopyFinished => 
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        requester ! CopyFinished
      } else {
        context become copyingBehaviour(requester, newExpected, insertConfirmed)
      }
      
    case OperationFinished(id) => 
      if (expected.isEmpty) {
        requester ! CopyFinished
      } else {
        context become copyingBehaviour(requester, expected, true)
      }
  }
  
  /**
   * The initial behaviour is the normal one
   */
  val receive: Receive = normalBehaviour

  // To get the Left or Right SubTree

  def getSubTree(x: Int): Position = {
    if (x < this.elem) {
      Left
    } else {
      Right
    }
  }
  
  // To create a new Actor

  def newNode(x: Int, removedInitially: Boolean): ActorRef =
    context.actorOf(props(x, removedInitially), "node-" + x)

  def createNewNode(pos: Position, x: Int): Unit = 
    subtrees += pos -> newNode(x, false)
  
  // Jobs
    
  trait Job {
    def whenNodeFound(req: ActorRef, id: Int): Unit
    def whenNoNodesFound(pos: Position, op: Operation): Unit
  }

  def performOperation(op: Operation, job: Job): Unit =
    op.elem match {
    
      // the element of the operation is found (op.elem == this.elem) 
      case this.elem =>
        job.whenNodeFound(op.requester, op.id)

      // the element of the operation can be found in one of the 
      // subtrees, or maybe it does not exist. 
      case _ =>
        val pos: Position = getSubTree(op.elem)
        val childNode: Option[ActorRef] = subtrees.get(pos)
        childNode match {
          case None => job.whenNoNodesFound(pos, op)
          case Some(childActor) => childActor ! op
        }
    }

  object InsertJob extends Job {
    def whenNodeFound(req: ActorRef, id: Int): Unit = {
      removed = false
      req ! OperationFinished(id)
    }
    def whenNoNodesFound(pos: Position, op: Operation): Unit = {
      createNewNode(pos, op.elem)
      op.requester ! OperationFinished(op.id)
    }
  }

  object ContainsJob extends Job {
    def whenNodeFound(req: ActorRef, id: Int): Unit =
      req ! ContainsResult(id, !removed)
    def whenNoNodesFound(pos: Position, op: Operation): Unit =
      op.requester ! ContainsResult(op.id, false)
  }

  object RemoveJob extends Job {
    def whenNodeFound(req: ActorRef, id: Int): Unit = {
      removed = true
      req ! OperationFinished(id)
    }
    def whenNoNodesFound(pos: Position, op: Operation): Unit =  
      op.requester ! OperationFinished(op.id)
  }
  
}
