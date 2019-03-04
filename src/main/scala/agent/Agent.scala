package agent


case class Rule(name: String, c: List[Condition], a: List[Fact]) {
  var ag: Agent = null
  var specificity = c.length
}
case class Action(f: () => Unit) {var ag: Agent = null}

object RuleGenerator {

  var ruleBase : List[Rule] = List[Rule]()

  def initialize(): List[Rule] = {
    ruleBase
  }

  def not(f: Fact): Fact = {
    f.value = false
    f
  }

  implicit def R(name: String) =
    new {
      def --(c: Condition): RuleDef = {
        new RuleDef(name, List(c),List())
      }
    }

  class RuleDef(n: String, conditions: List[Condition], actions: List[Fact]) {
    def &(c: Condition): RuleDef = {
      new RuleDef(n, c :: conditions,actions)
    }

    def |-->(f: Fact*): Rule = {
      new Rule(n,conditions,f.toList)
    }



    def addRule(r: Rule): Unit = {
      ruleBase = r :: ruleBase
    }

  }

}

trait Agent

abstract class ReflexAgent extends Agent {self=>

  var agentRules: List[Rule] = null
  var beliefs: Set[Fact] = null

  def subjectTo(rls : Rule*) : ReflexAgent = {
      new ReflexAgent {
        agentRules = rls.toList
        beliefs = self.beliefs
      }
  }


  def selectRule(): Rule = {
    var rls = evaluateRules()
    var rl: Rule = rls.head
    rls foreach {r=> {
      if (r.specificity > rl.specificity)
          rl = r
    }}
    rl
  }

  private def evaluateRules(): List[Rule] = {
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
  def apply(bels: Set[Fact]): ReflexAgent = {
    new ReflexAgent {
        //rules
        //agentRules = RuleGenerator.initialize()
        //RuleGenerator.ruleBase = List[Rule]()
        beliefs = bels
        beliefs foreach {b => b.ag=this}
    }
  }
}
