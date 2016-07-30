package kvstore

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

import Persistence.Persist
import Persistence.Persisted
import Replicator.Replicate
import Replicator.Replicated
import Replicator.Snapshot
import Replicator.sendSnapshotAck
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.actor.Props
import akka.actor.actorRef2Scala
import kvstore.Arbiter.Join
import kvstore.Arbiter.JoinedPrimary
import kvstore.Arbiter.JoinedSecondary
import kvstore.Arbiter.Replicas

object Replica {

  sealed trait Operation {
    def key: String
    def id: Long
  }

  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply

  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class TimeOutMsg(key: String, seq: Long)

  def props(arbiter: ActorRef, persistenceProps: Props): Props =
    Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {

  import GlobalAcknowledge._
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  var kv: Map[String, String] =
    Map.empty[String, String]

  var expectedSeq: Long = 0L

  var secondaries: Map[ActorRef, ActorRef] =
    Map.empty[ActorRef, ActorRef]

  var replicators: Set[ActorRef] =
    Set.empty[ActorRef]

  var persistenceActor: ActorRef =
    context.actorOf(persistenceProps)

  var persistenceAcks: Map[Long, (ActorRef, Persist)] =
    Map.empty[Long, (ActorRef, Persist)]

  case class GlobalAcknowledge(
    requesterActor: ActorRef,
    key: String,
    seq: Long,
    pendingReplicators: Set[ActorRef] = Set.empty,
    persistenceAck: Boolean = false) {

    def isFinished: Boolean =
      persistenceAck && pendingReplicators == Set.empty
  }

  object GlobalAcknowledge {

    var pendingAcks: Map[(String, Long), GlobalAcknowledge] =
      Map.empty[(String, Long), GlobalAcknowledge]

    private def checkFinished(ack: GlobalAcknowledge): Unit =
      if (ack.isFinished) {
        ack.requesterActor ! OperationAck(ack.seq)
        pendingAcks -= Pair(ack.key, ack.seq)
      } else {
        pendingAcks += (ack.key, ack.seq) -> ack
      }

    private def getGlobalAck(requester: ActorRef, key: String, seq: Long): GlobalAcknowledge =
      pendingAcks.getOrElse((key, seq),
        GlobalAcknowledge(requesterActor = requester, key = key, seq = seq))

    def confirmPersistence(key: String, seq: Long): Unit =
      pendingAcks.get(key, seq).foreach {
        (ack: GlobalAcknowledge) =>
          checkFinished(ack.copy(persistenceAck = true))
      }

    def ackReplication(replicator: ActorRef, key: String, seq: Long): Unit =
      pendingAcks.get(key, seq).foreach {
        ack: GlobalAcknowledge =>
          checkFinished(ack.copy(pendingReplicators = ack.pendingReplicators - replicator))
      }

    def failOperation(key: String, seq: Long): Unit =
      pendingAcks.get((key, seq)).foreach {
        ack: GlobalAcknowledge =>
          ack.requesterActor ! OperationFailed(seq)
          pendingAcks -= Pair(key, seq)
      }

    def startPersistence(requester: ActorRef, key: String, seq: Long): Unit =
      pendingAcks += Pair(key, seq) -> getGlobalAck(requester, key, seq).copy(persistenceAck = true)

    def startReplication(requester: ActorRef, replicator: ActorRef, key: String, seq: Long): Unit = {
      val currAck: GlobalAcknowledge = getGlobalAck(requester, key, seq)
      pendingAcks +=
        Pair(key, seq) -> currAck.copy(pendingReplicators = currAck.pendingReplicators + replicator)
    }

    def removeReplicator(oldReplicator: ActorRef): Unit =
      pendingAcks.values.foreach {
        ack: GlobalAcknowledge =>
          checkFinished(ack.copy(pendingReplicators = ack.pendingReplicators - oldReplicator))
      }
  }

  def lookUp(g: Get): Unit =
    sender ! GetResult(g.key, kv get g.key, g.id)

  def addEntry(key: String, value: String): Unit =
    kv += key -> value

  def removeEntry(key: String): Unit =
    kv -= key

  def sendReplicate(replicator: ActorRef, key: String, valueOption: Option[String], seq: Long): Unit =
    replicator ! Replicate(key, valueOption, seq)
    
  def receive: Receive = {
    case JoinedPrimary   => context become leaderBehaviour
    case JoinedSecondary => context.become(replicaBehaviour)
  }

  val casualID: Long = 12345678901234L

  def createNewReplicator(replicaNode: ActorRef): ActorRef =
    context.actorOf(Replicator.props(replicaNode))
  
  val leaderBehaviour: Receive = {

    case Replicas(replicaNodes: Set[ActorRef]) =>
      val oldReplicas: Set[ActorRef] = secondaries.keySet diff replicaNodes
      oldReplicas.foreach {
        (replica: ActorRef) =>
          val oldReplicator: ActorRef = secondaries.apply(replica)
          removeReplicator(oldReplicator)
          oldReplicator ! PoisonPill
          secondaries -= replica
      }
      val newReplicas: Set[ActorRef] = replicaNodes -- secondaries.keySet - self
      newReplicas foreach {
        replica: ActorRef =>
          val newReplicator: ActorRef = createNewReplicator(replica)
          secondaries += replica -> newReplicator
          kv foreach {
            case (key: String, value: String) =>
              sendReplicate(newReplicator, key, Some(value), casualID)
          }
      }
    
    case g @ Get(key: String, seq: Long) => lookUp(g)

    case t @ TimeOutMsg(_: String, _: Long) =>
      failOperation(t.key, t.seq)
    
    case Insert(key: String, value: String, seq: Long) =>
      addEntry(key, value)
      replicateAndPersist(key, Some(value), seq)

    case Remove(key: String, seq: Long) =>
      removeEntry(key)
      replicateAndPersist(key, None, seq)
      
    case Persisted(key: String, seq: Long) =>
      confirmPersistence(key, seq)

    case Replicated(key: String, seq: Long) =>
      ackReplication(sender, key, seq)
  }

  val replicaBehaviour: Receive = {

    case g @ Get(key: String, seq: Long) => lookUp(g)

    case Snapshot(key: String, valueOption: Option[String], seq: Long) =>
      if (seq == expectedSeq) {
        updateKVStore(key, valueOption)
        persistEntry(key, valueOption, seq)
        expectedSeq += 1
      } else if (seq < expectedSeq) {
        sendSnapshotAck(sender, key, seq)
      }

    case Persisted(key: String, seq: Long) =>
      sendSnapshotAck(persistenceAcks(seq)._1, key, seq)
      persistenceAcks -= seq
  }

  def updateKVStore(key: String, valueOption: Option[String]): Unit =
    if (valueOption.isDefined) {
      addEntry(key, valueOption.get)
    } else {
      removeEntry(key)
    }
  
  def replicateAndPersist(key: String, valueOption: Option[String], seq: Long): Unit = {
    secondaries.values foreach {
      (replicator: ActorRef) =>
        startReplication(sender, replicator, key, seq)
        sendReplicate(replicator, key, valueOption, seq)
    }
    startPersistence(sender, key, seq)
    persistEntry(key, valueOption, seq)
    context.system.scheduler.scheduleOnce(1 second, self, TimeOutMsg(key, seq))
  }

  def persistEntry(key: String, valueOption: Option[String], seq: Long): Unit = {
    val p: Persist = Persist(key, valueOption, seq)
    persistenceActor ! p
    persistenceAcks += seq -> (sender, p)
  }

  def repersistAll: Unit =
    persistenceAcks.foreach {
      case (_: Long, (_: ActorRef, p: Persist)) => persistenceActor ! p
    }

  /**
   * Method overridden from trait Actor.
   * It is called before the actor starts.
   */ 
  override def preStart: Unit = {
    context.system.scheduler.schedule(0 millis, 100 millis)(repersistAll)
    arbiter ! Join
  }

}
