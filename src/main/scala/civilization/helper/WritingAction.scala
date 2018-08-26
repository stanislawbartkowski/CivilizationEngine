package civilization.helper

import civilization.action.{Command, SuspendCommandTrait}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects._
import civilization.action._
import civilization.gameboard._
import civilization.message.{M, Mess}
import play.api.libs.json._

object WritingAction extends SuspendCommandTrait with ResourceActionTrait with ImplicitMiximFromJson with ImplicitMiximToJson {

  override val command: Command.T = Command.WRITINGACTION

  private def verifyAction(b: GameBoard, com : Command.T) : Option[Mess] = {
    val current: CurrentPhase = currentPhase(b)
    if (current.turnPhase != TurnPhase.CityManagement) Some(Mess(M.NOTALLOWEDINTHISPHASE,(current.turnPhase,command)))
    else if (Command.actionPhase(com) != TurnPhase.CityManagement) Some(Mess(M.EXPECTEDCITYACTIONTOBECANCELED,com))
    else if (TechnologyFeatures.isResourceAbilityAction(com)) Some(Mess(M.CANNOTCANCELRESOURCEABILITY,com))
    else None
  }

  override def canSuspend(board: GameBoard, deck: PlayerDeck, comm: Command): Unit = {
    if (!existResourceAndTech(board, deck, resource(board), techn)) return
    if (verifyAction(board,comm.command).isDefined) return
    board.addActionSuspend(deck,command,resource(board))
  }

  class WritingAction extends AbstractCommandNone {

    override def execute(board: GameBoard): Unit =
      decrResourceHVForTech(board, deck, resource(board), techn, isExecute)
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new WritingAction

}
