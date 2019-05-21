package agent


trait Sensor[Action,Percept] {
  def agent: Agent[Action,Percept]
  def perceive(environment: Environment[Action,Percept]): Percept
}


trait Actuator[Action, Percept] {
  def act(action: Action, environment: Environment[Action,Percept]): Environment[Action,Percept]
}


trait Environment[Action,Percept] {
  def addAgent(agent: Agent[Action, Percept]): Environment[Action, Percept]
  def removeAgent(agent: Agent[Action, Percept]): Environment[Action, Percept]
  def actuate(actuator: Actuator[Action, Percept], action: Action): Environment[Action, Percept]
  def perceive(sensor: Sensor[Action,Percept]): Percept
}

trait Agent[Action, Percept] {
  def perceive : Environment[Action, Percept] => Percept
  def action : Percept => Action = {p => defaultAction}
  def execute: Environment[Action,Percept] => Environment[Action,Percept] =
    e => actuator.act(action(sensor.perceive(e)),e)

  def sensor: Sensor[Action,Percept]
  def actuator: Actuator[Action, Percept]
  def defaultAction : Action
}


trait ReflexiveAgentWithState[Action, Percept, State] extends Agent[Action, Percept] {
  def actionByState : State => Action
  def next : (State,Percept) => State
  def initialState : State
  var state : State = initialState

  override def execute : Environment[Action,Percept] => Environment[Action,Percept] = {
    e  => {
      actuator.act(
        actionByState(
            next(state,
            sensor.perceive(e))),
        e)
    }
  }
}


trait ModelBasedReflexAgent[Action, Percept, State] extends Agent[Action, Percept] {
  type StateTransitionModel = (State, Action) => State
  type UpdateState = (State, Action, Percept, StateTransitionModel) => State
  type ActionSelector   = (State,Percept) => Action

  def initialState: State

  var state: State   = initialState // the agent's current conception of the world state
  var newAction: Action      //most recent action
  var percept: Percept

  override def execute : Environment[Action,Percept] => Environment[Action,Percept] = {
    e => {
      newAction = ruleMatch(state, sensor.perceive(e))
      state = updateState(state, newAction, percept, model)
      actuator.act(newAction, e)
    }
  }

  def rules: ActionSelector
  def model: StateTransitionModel

  def ruleMatch(state: State, percept: Percept): Action = rules(state,percept)

  def updateState: UpdateState
}



trait RuleBasedAgent[Action,Percept] extends Agent[Action,Percept] with PartialOrdering[Set[Percept]] {
  type Observation <: Seq[Percept]
  type Rule = Tuple2[Set[Percept],Action]
  type RuleBase = Map[Set[Percept],Action]
  type Evaluator = Environment[Action,Percept] => Set[Percept]
  type RuleFilter =  (Set[Percept], RuleBase) => Map[Set[Percept],Action]
  type RuleSelector = Map[Set[Percept],Action] => Rule


  var ruleBase: RuleBase
  var observation : Observation

  override def lteq(x: Set[Percept], y: Set[Percept]) : Boolean = {
    if (x.size <= y.size) true
    else false
  }

  override def tryCompare(x: Set[Percept], y: Set[Percept]): Option[Int] = Some(0)

  override def execute : Environment[Action,Percept] => Environment[Action,Percept] = {
    e => {
      actuator.act(
        selection(ruleFilter(evaluate(e),ruleBase))._2
        ,e)
    }
  }


  def evaluate : Evaluator
  def ruleFilter : RuleFilter
  def selection : RuleSelector
}




