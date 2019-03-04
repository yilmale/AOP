package agent

trait Condition

class Fact extends Condition {
  var ag : Agent = null
  var value : Boolean = true
}

abstract class BeliefBase {
  var facts: Set[Fact]
  def add(f: Fact): Unit =  {
    facts = facts + f
  }

}

object Facts {
  def apply(fs: Set[Fact]): Set[Fact] = {
    fs
  }
}
