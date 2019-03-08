import agent._


object AOPMain extends App  {

  case class Locked(d: String) extends Belief
  case class Acquired(r: String) extends Belief
  case class State(x: Int, y: Int) extends Belief


  println("AOP main driver")


  import RuleGenerator._

  class MyAgent extends ReflexAgent

  import ReflexAgent._
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


    var myf = Action(action1)
    myf.f()


    Model {
      Beliefs(Set(
        Locked("D1"),
        Acquired("R1")))
    } subjectTo (
            "Rule1" -- Locked("D1") & Acquired("R1") |--> {var x= a+d; a=12},
            "Rule1" -- Locked("D1") & Acquired("R1") |--> {var x= a+d; a=12}
      )

  }


}
