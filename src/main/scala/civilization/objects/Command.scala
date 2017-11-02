package civilization.objects

import play.api.libs.json.JsValue

/** Contains commands implemented */

object Command extends Enumeration {

  type T = Value
  val SETCAPITAL, SETCITY, ENDOFPHASE, STARTMOVE, MOVE, REVEALTILE, ENDOFMOVE,
  SETARMY, SETSCOUT, BUYARMY, BUYSCOUT, RESEARCH, FORCEDMOVEFIGURES, SPENDTRADE, UNDOSPENDTRADE,
  SENDPRODUCTION, UNDOSENDPRODUCTION, BUYINFANTRY, BUYMOUNTED, BUYARTILLERY, BUYAIRCRAFT,
  HARVESTRESOURCE, EXPLOREHUT, ATTACK = Value

  /** Assign action to game phases */
  def actionPhase(t: Value): TurnPhase.T = {
    t match {
      case SETCAPITAL | SETCAPITAL | SETARMY | SETSCOUT => TurnPhase.StartOfTurn
      case STARTMOVE | MOVE | ENDOFMOVE | REVEALTILE | EXPLOREHUT | ATTACK => TurnPhase.Movement
      case BUYARMY | BUYSCOUT | SPENDTRADE | UNDOSPENDTRADE | SENDPRODUCTION | UNDOSENDPRODUCTION |
           BUYMOUNTED | BUYINFANTRY | BUYAIRCRAFT | BUYARTILLERY | HARVESTRESOURCE => TurnPhase.CityManagement
      case RESEARCH => TurnPhase.Research
      case _ => null
    }
  }

  /** Unique city actions. */
  def cityActionUnique(t: Value): Boolean = (t == BUYARMY || t == BUYSCOUT || t == BUYARTILLERY ||
        t == BUYAIRCRAFT || t == BUYINFANTRY || t == BUYMOUNTED || t == HARVESTRESOURCE)

  /** Movement action after strating the move */
  def actionMove(t: Value): Boolean = {
    return t == MOVE || t == REVEALTILE || t == ENDOFMOVE || t == EXPLOREHUT || t == ATTACK
  }
}

case class CommandParams(val p: Option[P], val param: Option[JsValue])

case class CommandValues(val command: Command.T, val civ: Civilization.T, val p: P, val param: JsValue)


