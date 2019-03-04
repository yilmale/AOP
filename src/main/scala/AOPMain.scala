import agent._


object AOPMain extends App  {

  case class Locked(d: String) extends Fact
  case class Acquired(r: String) extends Fact
  case class State(x: Int, y: Int) extends Fact


  println("AOP main driver")


  import RuleGenerator._

  var a : Int =10
  var b = ReflexAgent (
      Facts(Set(
         Locked("D1"),
         Acquired("R1"),
         Acquired("R3")))
      ) subjectTo (
         "Rule1" -- Locked("D1") & State(a,a) & Acquired("R1") |--> (Locked("D1"),
                State(a+10,a),not(Locked("D3"))),
         "Rule2" -- Locked("D1") & Acquired("R2") |--> (Locked("D1"),Locked("D3"))
    )


  var rl = b.selectRule()
  println(rl.name + " specificity " + rl.specificity)
  rl.c foreach {cnd => println(cnd)}
  rl.a foreach {act => println(act)}

}
