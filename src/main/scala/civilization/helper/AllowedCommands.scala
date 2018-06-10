package civilization.helper

import civilization.action.CommandContainer
import civilization.gameboard.GameBoard
import civilization.helper.SetFigureAction.itemizeForSetBuyFigures
import civilization.helper.move.MoveItemize
import civilization.io.tojson._
import civilization.objects._
import play.api.libs.json.{JsArray, JsNull, JsValue, Json}

object AllowedCommands {

  // ========
  // external


  def allowedCommands(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    var cu: CurrentPhase = currentPhase(b)
    var co: Seq[Command.T] = Nil
    // if "turned" phase of the game, only current player has moves until completes
    if (TurnPhase.turnAction(cu.turnPhase) && civ != cu.notcompleted.head) return Nil
    // if player already completed return Nil
    // 2017/01/05
    if (!(cu.notcompleted contains civ)) return Nil
    co = CommandContainer.commandsAvail(b, civ, cu.turnPhase)
    cu.turnPhase match {
      case TurnPhase.StartOfTurn => {
          if (co.find(Command.isBeginningOfGameCommand(_)).isDefined) return co
      }
      case TurnPhase.Movement => {
        val r = MoveItemize.allowedCommands(b, civ)
        if (r._1) return co ++ r._2
        co = co ++ r._2
      }
      case _ => {}
    }
    val restricted : Seq[Command.T] = co.filter(Command.isBlockingCommand(_))
    if (!restricted.isEmpty) return restricted
    if (cu.notcompleted.find(_ == civ).isDefined) co = co ++ List(Command.ENDOFPHASE)
    co
  }

  def itemizeCommandS(b: GameBoard, civ: Civilization.T, command: Command.T): String = {
    if (Command.internalAction(command)) return null
    var pp: P = null
    var name: String = null
    var l: Seq[JsValue] = Nil
    if (CommandContainer.isCommandCovered(command))
      return Json.prettyPrint(JsArray(CommandContainer.itemize(b, civ, command)))
    if (Command.actionPhase(command) == TurnPhase.Movement) {
      val r = MoveItemize.itemizeCommand(b, civ, command)
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
