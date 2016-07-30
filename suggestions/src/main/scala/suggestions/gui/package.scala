package suggestions

import scala.swing.Reactions.Reaction
import scala.swing.Reactions.StronglyReferenced
import scala.swing.event.Event

package object gui {
  
  object Reaction {
    def apply(r: Reaction) = r
  }
}