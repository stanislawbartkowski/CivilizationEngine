package civilization.helper

import civilization.action.{AbstractCommand, AbstractCommandNone, Command, CommandPackage}
import civilization.gameboard.{BuildingPoint, GameBoard, PlayerDeck, PlayerTechnology}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GameResources
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.{action, gameboard, message, objects}
import civilization.objects._
import play.api.libs.json.JsValue


object LetSuspendedGoAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.LETSUSPENDEDGO)

  // does nothing, only place holder
  protected class LetSuspendedGo extends AbstractCommandNone {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      if (!isExecute) Mess(M.THISCOMMANDCANBEONLYINEXECUTEDMODE, command)
      else {
        val m: Option[Mess] = expectedSuspended(board)
        if (m.isEmpty) null
        else m.get
      }
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      setForgetCurrentCommand
      setExecuteSuspendedCommand
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new LetSuspendedGo

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = Nil

}
