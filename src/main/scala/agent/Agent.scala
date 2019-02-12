package agent


case class Rule(name: String, c: List[Condition], a: Action)
case class Action(f: () => Unit)

object RuleGenerator {

  var ruleBase : List[Rule] = List[Rule]()

  def initialize(): List[Rule] = {
    ruleBase
  }

  implicit def R(name: String) =
    new {
      def --(c: Condition): RuleDef = {
        new RuleDef(name, List(c))
      }
    }

  class RuleDef(n: String, conditions: List[Condition]) {
    def &(c: Condition): RuleDef = {
      new RuleDef(n, c :: conditions)
    }

    def |-->(stmt: => Unit) {
      addRule(Rule(n,conditions,Action(()=>stmt)))
    }

    def addRule(r: Rule): Unit = {
      ruleBase = r :: ruleBase
    }

  }

}


abstract class Agent() {

  var agentRules: List[Rule] = RuleGenerator.initialize()
  var beliefs: Set[Fact]

  def evaluateRules(): Unit = {
    for (x <- agentRules) {
      var matched = true
      var cndIterator = x.c.iterator
      while ((cndIterator.hasNext) && (matched == true)) {
        var cnd = cndIterator.next()
        if (beliefs contains cnd.asInstanceOf[Fact]) matched = true
        else matched = false
      }
      if (matched==true) println(x.name + " is satisfied")
          else println(x.name + " is not satisfied")
      }
    }
}

object Agent {
  def apply(bels: Set[Fact])(rules : => Unit): Agent = {
    new Agent() {
        rules
        agentRules = RuleGenerator.initialize()
        RuleGenerator.ruleBase = List[Rule]()
        var beliefs = bels
    }
  }
}
