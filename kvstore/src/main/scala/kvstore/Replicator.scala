package kvstore

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala

object Replicator {

  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def sendSnapshotAck(replica: ActorRef, key: String, seq: Long): Unit =
    replica ! SnapshotAck(key, seq)

  def props(replica: ActorRef): Props =
    Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {

  import Replicator._
  import Replica._
  import context.dispatcher

  var acks: Map[Long, (ActorRef, Replicate)] =
    Map.empty[Long, (ActorRef, Replicate)]

  var _seqCounter: Long = 0L

  def nextSeq: Long = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  def sendSnapshot(rep: Replicate, seq: Long): Unit =
    replica ! Snapshot(rep.key, rep.valueOption, seq)

  def receive: Receive = {

    case rep @ Replicate(key: String, valueOption: Option[String], _: Long) =>
      val seq: Long = nextSeq
      sendSnapshot(rep, seq)
      acks += seq -> (sender, rep)

    case SnapshotAck(key: String, seq: Long) =>
      acks get (seq) foreach {
        case (requester: ActorRef, rep: Replicate) =>
          acks -= seq
          requester ! Replicated(key, rep.id)
      }
  }

  def resend: Unit =
    acks.foreach {
      case (seq: Long, (_: ActorRef, rep: Replicate)) =>
        sendSnapshot(rep, seq)
    }

  /**
   * Method overridden from trait Actor.
   * It is called before the actor starts.
   */ 
  override def preStart: Unit =
    context.system.scheduler.schedule(0 millis, 100 millis)(resend)
}
