package agent


trait Percept

trait Action

trait Sensor {
  def agent: Agent
  def perceive(environment: Environment): Percept
}

trait Actuator {
  def agent: Agent
  def act(action: Action, environment: Environment): Environment
}




trait Environment {
  def addAgent(agent: Agent): Environment
  def removeAgent(agent: Agent): Environment
  def actuate(actuator: Actuator, action: Action): Environment
  def perceive(sensor: Sensor): Percept
}

trait Agent {
  type Perception = PartialFunction[Environment,Percept]
  def perceive : Perception
  type AgentAction = Environment => Action
  def agentFunction: AgentAction
}



trait ReflexiveAgent extends Agent {
  def action : PartialFunction[Seq[Percept],Action]
  def observe : PartialFunction[Perception,Seq[Percept]]
  lazy val agentFunction : AgentAction = { e =>
    action(observe(perceive))
  }
}


trait ReflexiveAgentWithState extends Agent {
  type State
  def action : PartialFunction[State,Action]
  def next : PartialFunction[(State,Percept),State]
  def initialState : State
  var state : State = initialState


  lazy val agentFunction : AgentAction = { e => {
    state = next(state, perceive(e))
    action(state)
  }
  }

}


trait ModelBasedReflexAgent extends Agent {

}






