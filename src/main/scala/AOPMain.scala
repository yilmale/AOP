import agent._


object AOPMain extends App  {

  case class Locked(d: String) extends Condition
  case class Acquired(r: String) extends Condition
  case class State(x: Int, y: Int) extends Condition


  println("AOP main driver")


  import RuleGenerator._

  class MyAgent extends ReflexAgent

  var c = new MyAgent
  {
    var a = 10
    var d = 15

    val action1 = () => {
      var x = a+d
      println("Action1")
    }

    val action2 = () => {
      println("Action2")
    }


    Model (
      Conditions(Set(
        Locked("D1"),
        Acquired("R1"))),
      Actions(Set(
        action1,action2))
    ) subjectTo (
            "Rule1" -- Locked("D1") & Acquired("R1") |--> {var x= a+d; action1()},
            "Rule1" -- Locked("D1") & Acquired("R1") |--> {var x= a+d; action2()}
      )

  }


}
