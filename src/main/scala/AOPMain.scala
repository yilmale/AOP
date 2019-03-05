import agent._


object AOPMain extends App  {

  case class Locked(d: String) extends Belief
  case class Acquired(r: String) extends Belief
  case class State(x: Int, y: Int) extends Belief


  println("AOP main driver")


  import RuleGenerator._

  class MyAgent(var x: Int,bels:Set[Belief]) extends ReflexAgent(Beliefs(bels))



  var b = new MyAgent(x=5,
      Beliefs(Set(
         Locked("D1"),
         Acquired("R1"),
         Acquired("R3"),
         State(10,10)))
      ) subjectTo (
         "Rule1" -- Locked("D1") & Acquired("R1") |--> {},
         "Rule2" -- Locked("D1") & Acquired("R2") |--> {}
      )


  var rl = b.selectRule()
  println(rl.name + " specificity " + rl.specificity)
  rl.c foreach {cnd => println(cnd)}


}
