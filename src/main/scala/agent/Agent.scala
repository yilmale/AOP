package agent


case class Rule1(name: String, c: List[Condition], a: List[Belief]) {
  var ag: Agent = null
  var specificity = c.length
}

case class Rule(name: String, c: List[Condition], a: Action) {
  var ag: Agent = null
  var specificity = c.length
}


case class Action(f: ()=>Unit)

object RuleGenerator {

  var ruleBase : List[Rule] = List[Rule]()

  def initialize(): List[Rule] = {
    ruleBase
  }

  def not(f: Belief): Belief = {
    f.value = false
    f
  }

  implicit def R(name: String) =
    new {
      def --(c: Condition): RuleDef = {
        new RuleDef(name, List(c),List())
      }
    }

  class RuleDef(n: String, conditions: List[Condition], actions: List[Belief]) {
    def &(c: Condition): RuleDef = {
      new RuleDef(n, c :: conditions,actions)
    }

    def |-->(f: Belief*): Rule1 = {
      new Rule1(n,conditions,f.toList)
    }

    def |-->(stmt : => Unit): Rule = {
      new Rule(n,conditions,Action(() => stmt))
    }



    def addRule(r: Rule): Unit = {
      ruleBase = r :: ruleBase
    }

  }

}

trait Agent

abstract class ReflexAgent extends Agent {self=>

  import ReflexAgent._
  var model : ReflexModel = null

  def this(bels: Set[Belief]) {
      this
      model = new ReflexModel(bels)
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
    for (x <- model.agentRules) {
      var matched = true
      var cndIterator = x.c.iterator
      while ((cndIterator.hasNext) && (matched == true)) {
        var cnd = cndIterator.next()
        if (model.beliefs contains cnd.asInstanceOf[Belief]) matched = true
        else matched = false
      }
      if (matched==true) matchRules = x :: matchRules

      }
    matchRules
    }
}

object ReflexAgent {

  class ReflexModel(var beliefs: Set[Belief], var agentRules: List[Rule]=null) {
    def subjectTo(rls: Rule*): ReflexModel = {
      new ReflexModel(this.beliefs,rls.toList)
    }
  }

  def apply(bels: Set[Belief]): ReflexAgent = {
    new ReflexAgent {
        //rules
        //agentRules = RuleGenerator.initialize()
        //RuleGenerator.ruleBase = List[Rule]()
        //beliefs = bels
        //beliefs foreach {b => b.ag=this}
    }
  }

  def Model(bels:Set[Belief]): ReflexModel = {
    new ReflexModel(bels)
  }

}
