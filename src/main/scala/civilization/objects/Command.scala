package civilization.objects

import play.api.libs.json.JsValue

/** Contains commands implemented */

object Command extends Enumeration {

  type T = Value
  val SETCAPITAL, SETCITY, ENDOFPHASE, STARTMOVE, MOVE, REVEALTILE, ENDOFMOVE,
  SETARMY, SETSCOUT, BUYARMY, BUYSCOUT, RESEARCH, FORCEDMOVEFIGURES, SPENDTRADE, UNDOSPENDTRADE,
  SENDPRODUCTION, UNDOSENDPRODUCTION, BUYINFANTRY, BUYMOUNTED, BUYARTILLERY, BUYAIRCRAFT,
  HARVESTRESOURCE, EXPLOREHUT, ATTACK, STARTBATTLE, PLAYUNIT, PLAYUNITIRON, ENDBATTLE, TAKEUNIT, TAKEWINNERLOOT,
  BUYBUILDING, BUYWONDER, POTTERYACTION, PHILOSOPHYACTION,DEVOUTTOCULTURE = Value

  /** Assign action to game phases */
  def actionPhase(t: Value): TurnPhase.T = {
    t match {
      case SETCAPITAL | SETCITY | SETARMY | SETSCOUT => TurnPhase.StartOfTurn
      case STARTMOVE | MOVE | ENDOFMOVE | REVEALTILE | EXPLOREHUT | ATTACK | STARTBATTLE | PLAYUNIT | PLAYUNITIRON | ENDBATTLE => TurnPhase.Movement
      case BUYARMY | BUYSCOUT | SPENDTRADE | UNDOSPENDTRADE | SENDPRODUCTION | UNDOSENDPRODUCTION |
           BUYMOUNTED | BUYINFANTRY | BUYAIRCRAFT | BUYARTILLERY | HARVESTRESOURCE | BUYBUILDING | BUYWONDER | POTTERYACTION | PHILOSOPHYACTION | DEVOUTTOCULTURE => TurnPhase.CityManagement
      case RESEARCH => TurnPhase.Research
      case _ => null
    }
  }

  /** Unique city actions. */
  def cityActionUnique(t: Value): Boolean = (t == BUYARMY || t == BUYSCOUT || t == BUYARTILLERY ||
    t == BUYAIRCRAFT || t == BUYINFANTRY || t == BUYMOUNTED || t == HARVESTRESOURCE || t == BUYBUILDING || t == BUYWONDER || t == POTTERYACTION || t == PHILOSOPHYACTION || t == DEVOUTTOCULTURE)

  /** Movement action after starting the move */
  def actionMove(t: Value): Boolean =
    return t == MOVE || t == REVEALTILE || t == ENDOFMOVE || t == EXPLOREHUT || t == ATTACK || t == STARTBATTLE || t == PLAYUNIT || t == PLAYUNITIRON || t == ENDBATTLE


  def isTechnologyResourceAction(t : Value) : Boolean =
    return (t == POTTERYACTION || t == PHILOSOPHYACTION)

  def isBeginningOfGameCommand(t : Value) : Boolean =
    (t == SETCAPITAL || t == SETARMY || t == SETSCOUT)

}

case class CommandParams(val p: Option[P], val param: Option[JsValue])

case class CommandValues(val command: Command.T, val civ: Civilization.T, val p: P, val param: JsValue)


