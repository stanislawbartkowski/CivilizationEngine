package civilization.objects

import play.api.libs.json.JsValue

/** Contains commands implemented */

object Command extends Enumeration {

  type T = Value
  val SETCAPITAL, SETCITY, ENDOFPHASE, STARTMOVE, MOVE, REVEALTILE, ENDOFMOVE, KILLFIGURE,
  SETARMY, SETSCOUT, BUYARMY, BUYSCOUT, RESEARCH, FORCEDMOVEFIGURES, SPENDTRADE, UNDOSPENDTRADE,
  SENDPRODUCTION, UNDOSENDPRODUCTION, BUYINFANTRY, BUYMOUNTED, BUYARTILLERY, BUYAIRCRAFT,
  HARVESTRESOURCE, EXPLOREHUT, ATTACK, STARTBATTLE, PLAYUNIT, PLAYUNITIRON, ENDBATTLE, TAKEUNIT,
  BUYBUILDING, BUYWONDER, POTTERYACTION, PHILOSOPHYACTION, DEVOUTTOCULTURE, ADVANCECULTURE, CURRENCYACTION, GREATPERSON, CULTURECARD,
  DISCARDCARD, GREATPERSONPUTNOW, GREATPERSONPUTNOWRESIGN, GREATPERSONPUT, BUILDCITYWALLFORFREE, BUYCITYWALL, ADVANCECULTUREFORFREE,
  GETCULTURE, SAVEUNIT, RANDOMWONDER, FREEWONDER, FREEBUILDINGCITYACTION, GETFREERESOURCE, DROPRESOURCE, GETRESOURCE,
  SACRIFICEFIGUREFORTECH, RESEARCHFREETECHNOLOGY, FREENONUPGRADEDBUILDING, TAKEFREEALLRESOURCESFROMMARKET, USESILKFORTRADE9, INCREASETRADE,
  INCREASEPRODUCTION, CONSTRUCTIONACTION, METALCASTINGACTION, BANKINGACTION, CHIVALRYACTION, DROPHUTVILLAGE, GETHUTVILLAGE,
  DROPCULTURECARD, DROPCOINFROMTECHNOLOGY, GETCOIN, GETTECHNOLOGY, CITYLOST  = Value

  /** Assign action to game phases */
  def actionPhase(t: Value): TurnPhase.T = {
    t match {
      case SETCAPITAL | SETCITY | SETARMY | SETSCOUT | GREATPERSONPUT | FREEWONDER | RANDOMWONDER => TurnPhase.StartOfTurn
      case STARTMOVE | MOVE | ENDOFMOVE | REVEALTILE | EXPLOREHUT | ATTACK | STARTBATTLE | PLAYUNIT | PLAYUNITIRON | ENDBATTLE | KILLFIGURE | SAVEUNIT | SACRIFICEFIGUREFORTECH | FREENONUPGRADEDBUILDING => TurnPhase.Movement
      case BUYARMY | BUYSCOUT | SPENDTRADE | UNDOSPENDTRADE | SENDPRODUCTION | UNDOSENDPRODUCTION |
           BUYMOUNTED | BUYINFANTRY | BUYAIRCRAFT | BUYARTILLERY | HARVESTRESOURCE | BUYBUILDING | BUYWONDER | POTTERYACTION | PHILOSOPHYACTION | DEVOUTTOCULTURE | ADVANCECULTURE | CURRENCYACTION | DISCARDCARD | GREATPERSONPUTNOW | GREATPERSONPUTNOWRESIGN |
           BUYCITYWALL | FREEBUILDINGCITYACTION | CONSTRUCTIONACTION | METALCASTINGACTION | BANKINGACTION | CHIVALRYACTION => TurnPhase.CityManagement
      case RESEARCH | GETFREERESOURCE => TurnPhase.Research
      case USESILKFORTRADE9 => TurnPhase.Trade
      case _ => null
    }
  }

  def internalAction(t: Value): Boolean = t == Command.ADVANCECULTUREFORFREE || t == FORCEDMOVEFIGURES || t == TAKEUNIT ||
    t == BUILDCITYWALLFORFREE || t == ADVANCECULTUREFORFREE || t == RANDOMWONDER || t == GETCULTURE || t == RESEARCHFREETECHNOLOGY || t == TAKEFREEALLRESOURCESFROMMARKET ||
    t == INCREASETRADE || t == INCREASEPRODUCTION || t == GETRESOURCE || t == DROPRESOURCE || t == GETHUTVILLAGE || t == DROPHUTVILLAGE || t == DROPCULTURECARD ||
    t == DROPCOINFROMTECHNOLOGY || t == GETCOIN || t == GETTECHNOLOGY || t ==  CITYLOST

  def commandNotPoint(t: Value): Boolean = (t == SAVEUNIT) || (t == PLAYUNIT) || (t == PLAYUNITIRON)

  /** Unique city actions. */
  def cityActionUnique(t: Value): Boolean = (t == BUYARMY || t == BUYSCOUT || t == BUYARTILLERY ||
    t == BUYAIRCRAFT || t == BUYINFANTRY || t == BUYMOUNTED || t == HARVESTRESOURCE || t == BUYBUILDING || t == BUYWONDER || t == POTTERYACTION || t == PHILOSOPHYACTION || t == DEVOUTTOCULTURE || t == CURRENCYACTION || t == BUYCITYWALL || t == FREEBUILDINGCITYACTION || t == CONSTRUCTIONACTION || t == METALCASTINGACTION || t == BANKINGACTION || t == CHIVALRYACTION)

  def actionInCity(t: Value): Boolean = (t != ADVANCECULTURE) && (t != DISCARDCARD) && (t != GREATPERSONPUTNOWRESIGN)

  /** Movement action after starting the move */
  def actionMove(t: Value): Boolean =
    return t == MOVE || t == REVEALTILE || t == ENDOFMOVE || t == EXPLOREHUT || t == ATTACK || t == STARTBATTLE || t == PLAYUNIT || t == PLAYUNITIRON || t == ENDBATTLE || t == KILLFIGURE || t == SAVEUNIT || t == FREENONUPGRADEDBUILDING

  def isTechnologyResourceAction(t: Value): Boolean =
    return t == POTTERYACTION || t == PHILOSOPHYACTION || t == CURRENCYACTION || t == CONSTRUCTIONACTION || t == METALCASTINGACTION || t == BANKINGACTION || t == CHIVALRYACTION

  def isBeginningOfGameCommand(t: Value): Boolean =
    (t == SETCAPITAL || t == SETARMY || t == SETSCOUT)

  def isBlockingCommand(t: Value): Boolean =
    return t == DISCARDCARD || t == GREATPERSONPUTNOW || t == GREATPERSONPUTNOWRESIGN || t == GETFREERESOURCE
}

case class CommandParams(val p: Option[P], val param: Option[JsValue])

case class CommandValues(val command: Command.T, val civ: Civilization.T, val p: P, val param: JsValue)


