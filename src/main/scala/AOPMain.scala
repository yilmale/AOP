import agent._


object AOPMain extends App  {

  case class Locked(d: String) extends Fact
  case class Acquired(r: String) extends Fact

  println("AOP main driver")


  import RuleGenerator._


  var b = Agent (
      Facts(Set(Locked("D1"),
         Acquired("R1"),
         Acquired("R3")))
    )
    {

      var x : Int = 0
      var y : Int = 0

      "Rule1" -- Locked("D1") & Acquired("R1")       |--> {x = x + 1}
      "Rule2" -- Acquired("R2")                      |--> {y = y + 10}
   }

  b.evaluateRules()

}
