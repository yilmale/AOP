package agent


case class Rule(name: String, c: List[Condition], a: Action) {var ag: Agent = null}
case class Action(f: () => Unit) {var ag: Agent = null}

object RuleGenerator {

  var ruleBase : List[Rule] = List[Rule]()

  def initialize(): List[Rule] = {
    ruleBase
  }

  def not(f: Fact): Unit = {

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

trait Agent

abstract class ReflexAgent extends Agent {

  var agentRules: List[Rule] = RuleGenerator.initialize()
  var beliefs: Set[Fact]


  def evaluateRules(): List[Rule] = {
    var matchRules = List[Rule]()
    for (x <- agentRules) {
      var matched = true
      var cndIterator = x.c.iterator
      while ((cndIterator.hasNext) && (matched == true)) {
        var cnd = cndIterator.next()
        if (beliefs contains cnd.asInstanceOf[Fact]) matched = true
        else matched = false
      }
      if (matched==true) matchRules = x :: matchRules

      }
    matchRules
    }
}

object ReflexAgent {
  def apply(bels: Set[Fact])(rules : => Unit): ReflexAgent = {
    new ReflexAgent() {
        rules
        agentRules = RuleGenerator.initialize()
        RuleGenerator.ruleBase = List[Rule]()
        var beliefs = bels
        agentRules foreach {r => {
          r.ag=this
          r.a.ag = this
        }}
        beliefs foreach {b => b.ag=this}
    }
  }
}
