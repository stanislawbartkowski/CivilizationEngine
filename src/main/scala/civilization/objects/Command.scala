package civilization.objects

import play.api.libs.json.JsValue

/** Contains commands implemented */

object Command extends Enumeration {

  type T = Value
  val SETCAPITAL, SETCITY, ENDOFPHASE, STARTMOVE, MOVE, REVEALTILE, ENDOFMOVE, KILLFIGURE,
  SETARMY, SETSCOUT, BUYARMY, BUYSCOUT, RESEARCH, FORCEDMOVEFIGURES, SPENDTRADE, UNDOSPENDTRADE,
  SENDPRODUCTION, UNDOSENDPRODUCTION, BUYINFANTRY, BUYMOUNTED, BUYARTILLERY, BUYAIRCRAFT,
  HARVESTRESOURCE, EXPLOREHUT, ATTACK, STARTBATTLE, PLAYUNIT, PLAYUNITIRON, ENDBATTLE, TAKEUNIT, TAKEWINNERLOOT,
  BUYBUILDING, BUYWONDER, POTTERYACTION, PHILOSOPHYACTION, DEVOUTTOCULTURE, ADVANCECULTURE, CURRENCYACTION, GREATPERSON, CULTURECARD,
  DISCARDCARD, GREATPERSONPUTNOW,GREATPERSONPUTNOWRESIGN = Value

  /** Assign action to game phases */
  def actionPhase(t: Value): TurnPhase.T = {
    t match {
      case SETCAPITAL | SETCITY | SETARMY | SETSCOUT => TurnPhase.StartOfTurn
      case STARTMOVE | MOVE | ENDOFMOVE | REVEALTILE | EXPLOREHUT | ATTACK | STARTBATTLE | PLAYUNIT | PLAYUNITIRON | ENDBATTLE | KILLFIGURE => TurnPhase.Movement
      case BUYARMY | BUYSCOUT | SPENDTRADE | UNDOSPENDTRADE | SENDPRODUCTION | UNDOSENDPRODUCTION |
           BUYMOUNTED | BUYINFANTRY | BUYAIRCRAFT | BUYARTILLERY | HARVESTRESOURCE | BUYBUILDING | BUYWONDER | POTTERYACTION | PHILOSOPHYACTION | DEVOUTTOCULTURE | ADVANCECULTURE | CURRENCYACTION | DISCARDCARD | GREATPERSONPUTNOW | GREATPERSONPUTNOWRESIGN => TurnPhase.CityManagement
      case RESEARCH => TurnPhase.Research
      case _ => null
    }
  }

  /** Unique city actions. */
  def cityActionUnique(t: Value): Boolean = (t == BUYARMY || t == BUYSCOUT || t == BUYARTILLERY ||
    t == BUYAIRCRAFT || t == BUYINFANTRY || t == BUYMOUNTED || t == HARVESTRESOURCE || t == BUYBUILDING || t == BUYWONDER || t == POTTERYACTION || t == PHILOSOPHYACTION || t == DEVOUTTOCULTURE || t == CURRENCYACTION)

  def actionInCity(t: Value): Boolean = (t != ADVANCECULTURE) && (t != DISCARDCARD) && (t != GREATPERSONPUTNOWRESIGN)

  /** Movement action after starting the move */
  def actionMove(t: Value): Boolean =
    return t == MOVE || t == REVEALTILE || t == ENDOFMOVE || t == EXPLOREHUT || t == ATTACK || t == STARTBATTLE || t == PLAYUNIT || t == PLAYUNITIRON || t == ENDBATTLE || t == KILLFIGURE


  def isTechnologyResourceAction(t: Value): Boolean =
    return (t == POTTERYACTION || t == PHILOSOPHYACTION || t == CURRENCYACTION)

  def isBeginningOfGameCommand(t: Value): Boolean =
    (t == SETCAPITAL || t == SETARMY || t == SETSCOUT)

  def isBlockingCommand(t: Value): Boolean =
    return t == DISCARDCARD || t == GREATPERSONPUTNOW || t == GREATPERSONPUTNOWRESIGN
}

case class CommandParams(val p: Option[P], val param: Option[JsValue])

case class CommandValues(val command: Command.T, val civ: Civilization.T, val p: P, val param: JsValue)


