package agent

trait Condition

class Belief extends Condition {
  var ag : Agent = null
  var value : Boolean = true
}

abstract class BeliefBase {
  var facts: Set[Belief]
  def add(f: Belief): Unit =  {
    facts = facts + f
  }

}

object Beliefs {
  def apply(fs: Set[Belief]): Set[Belief] = {
    fs
  }
}
