package civilization.helper

import civilization.action.CommandContainer
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.helper.SetFigureAction.itemizeForSetBuyFigures
import civilization.helper.move.MoveItemize
import civilization.io.tojson._
import civilization.objects._
import play.api.libs.json.{JsArray, JsNull, JsValue, Json}

object AllowedCommands {

  // ========
  // external


  def allowedCommands(b: GameBoard, deck: PlayerDeck): Seq[Command.T] = {
    // end of game
    if (b.endofgame.isDefined) return Nil
    if (b.isSuspended) return Nil
    var cu: CurrentPhase = currentPhase(b)
    var co: Seq[Command.T] = Nil
    // if "turned" phase of the game, only current player has moves until completes
    if (TurnPhase.turnAction(cu.turnPhase) && deck.civ != cu.notcompleted.head) return Nil
    // if player already completed return Nil
    // 2017/01/05
    if (!(cu.notcompleted contains deck.civ)) return Nil
    co = CommandContainer.commandsAvail(b, deck, cu.turnPhase)
    cu.turnPhase match {
      case TurnPhase.StartOfTurn => {
          if (co.find(Command.isBeginningOfGameCommand(_)).isDefined) return co
      }
      case TurnPhase.Movement => {
        val r = MoveItemize.allowedCommands(b, deck)
        if (r._1) return co ++ r._2
        co = co ++ r._2
      }
      case _ => {}
    }
    val restricted : Seq[Command.T] = co.filter(Command.isBlockingCommand(_))
    if (!restricted.isEmpty) return restricted
    if (cu.notcompleted contains deck.civ) co = co ++ List(Command.ENDOFPHASE)
    co
  }

  def itemizeCommandS(b: GameBoard, deck : PlayerDeck, command: Command.T): String = {
    if (Command.internalAction(command)) return null
    var pp: P = null
    var name: String = null
    var l: Seq[JsValue] = Nil
    if (CommandContainer.isCommandCovered(command))
      return Json.prettyPrint(JsArray(CommandContainer.itemize(b, deck, command)))
    if (Command.actionPhase(command) == TurnPhase.Movement) {
      val r = MoveItemize.itemizeCommand(b, deck, command)
      pp = r._2
      name = r._1
      l = r._3
    }
    else
      command match {
        case _ => None
      }
    if (pp == null) Json.prettyPrint(JsArray(l))
    else Json.prettyPrint(Json.obj(S.p -> writesP(pp), name -> JsArray(l)))
  }
}
