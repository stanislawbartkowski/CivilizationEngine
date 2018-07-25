package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.Mess
import civilization.objects._
import play.api.libs.json.JsValue

object WinTheGame extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.PLAYERWIN, Command.ENDOFGAME)

  protected class WinTheGame(override val param: GameWinType.T) extends AbstractCommand(param) {

    override protected def execute(board: GameBoard): Unit = {
      deck.winthegame = Some(param)
      board.addForcedCommandC(Command.ENDOFGAME, deck, null, param)
    }
  }

  protected class EndOfGameCommand(override val param: GameWinType.T) extends AbstractCommand(param) {

    override protected def execute(board: GameBoard): Unit =
      board.endofgame = Some(EndOfGame(deck,param))
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command =
    command match {
      case Command.PLAYERWIN => new WinTheGame(param)
      case Command.ENDOFGAME => new EndOfGameCommand(param)
    }

}

