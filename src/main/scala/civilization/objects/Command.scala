package civilization.objects

import play.api.libs.json.JsValue

object Command extends Enumeration {

  type T = Value
  val SETCAPITAL, SETCITY, ENDOFPHASE, STARTMOVE, MOVE, REVEALTILE, ENDOFMOVE,
  SETARMY, SETSCOUT, BUYARMY, BUYSCOUT, RESEARCH, FORCEDMOVEFIGURES, SPENDTRADE, UNDOSPENDTRADE,
  SENDPRODUCTION, UNDOSENDPRODUCTION, BUYINFANTRY, BUYMOUNTED, BUYARTILLERY, BUYAIRCRAFT = Value

  def actionPhase(t: Value): TurnPhase.T = {
    t match {
      case SETCAPITAL | SETCAPITAL | SETARMY | SETSCOUT => TurnPhase.StartOfTurn
      case STARTMOVE | MOVE | ENDOFMOVE | REVEALTILE => TurnPhase.Movement
      case BUYARMY | BUYSCOUT | SPENDTRADE | UNDOSPENDTRADE | SENDPRODUCTION | UNDOSENDPRODUCTION |
           BUYMOUNTED | BUYINFANTRY | BUYAIRCRAFT | BUYARTILLERY => TurnPhase.CityManagement
      case RESEARCH => TurnPhase.Research
      case _ => null
    }
  }

  def cityActionUnique(t: Value): Boolean = (t == BUYARMY || t == BUYSCOUT || t == BUYARTILLERY ||
        t == BUYAIRCRAFT || t == BUYINFANTRY || t == BUYMOUNTED)

  def actionMove(t: Value): Boolean = {
    return t == MOVE || t == REVEALTILE || t == ENDOFMOVE
  }
}

case class CommandParams(val p: Option[P], val param: Option[JsValue])

case class CommandValues(val command: Command.T, val civ: Civilization.T, val p: P, val param: JsValue)


