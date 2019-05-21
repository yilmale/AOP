package vacuumWorld

import agent._

trait VacuumAction

case class Forward() extends VacuumAction

case class NoAction() extends VacuumAction

trait VacuumPercept

case class Dirt(x: Int, y: Int) extends VacuumPercept
case class In(x: Int, y: Int) extends VacuumPercept
case class Facing(d: String) extends VacuumPercept
case object DefaultPercept extends VacuumPercept

case object VacuumSensor extends Sensor[VacuumAction,VacuumPercept] {
  override def agent: Agent[VacuumAction, VacuumPercept] = null
  override def perceive(environment: Environment[VacuumAction, VacuumPercept]): VacuumPercept = null
}

case object VacuumActuator extends Actuator[VacuumAction,VacuumPercept] {
  override def act(action: VacuumAction, environment: Environment[VacuumAction, VacuumPercept]):
        Environment[VacuumAction, VacuumPercept] = null
}


class VacuumWorldAgent extends RuleBasedAgent[VacuumAction, VacuumPercept] {

  override def evaluate: Environment[VacuumAction,VacuumPercept] => Set[VacuumPercept] =
      {ve  => Set[VacuumPercept]()}
  override def ruleFilter: (Set[VacuumPercept], RuleBase) => Map[Set[VacuumPercept],VacuumAction] =
    {(vps,rb) => Map[Set[VacuumPercept],VacuumAction]()}
  override def selection: Map[Set[VacuumPercept],VacuumAction] => Tuple2[Set[VacuumPercept],VacuumAction] =
  {rmap => (Set[VacuumPercept](),NoAction())}
  override def perceive: Environment[VacuumAction, VacuumPercept] => VacuumPercept = {e => DefaultPercept}

  var ruleBase: Map[Set[VacuumPercept],VacuumAction] = Map(
    Set() -> NoAction()
  )

  type Observation = List[VacuumPercept]

  var observation = List[VacuumPercept]()
  override val sensor = VacuumSensor
  override val actuator = VacuumActuator
  override val defaultAction = NoAction()

}


