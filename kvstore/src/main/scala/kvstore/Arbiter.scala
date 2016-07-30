package kvstore

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.actorRef2Scala

object Arbiter {
  case object Join

  case object JoinedPrimary
  case object JoinedSecondary

  /**
   * This message contains all replicas currently known to the arbiter, including the primary.
   */
  case class Replicas(replicas: Set[ActorRef])
}

class Arbiter extends Actor {

  import Arbiter._
  
  var leader: Option[ActorRef] = None
  
  var replicas: Set[ActorRef] = Set.empty[ActorRef]

  def receive: Receive = {

    case Join =>
      leader match {
        case None =>
          leader = Some(sender)
          replicas += sender
          sender ! JoinedPrimary
        case _ =>
          replicas += sender
          sender ! JoinedSecondary
      }
      
      leader foreach {
        _ ! Replicas(replicas)
      }
  }
  
}
