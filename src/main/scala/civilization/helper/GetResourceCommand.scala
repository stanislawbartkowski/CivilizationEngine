package civilization.helper

import civilization.action.{AbstractCommand, AbstractCommandNone, Command, CommandPackage}
import civilization.gameboard._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{FatalError, M, Mess}
import civilization.objects._
import civilization.{gameboard, message}
import play.api.libs.json.JsValue

object GetResourceCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.GETRESOURCE, Command.DROPRESOURCE)

  protected class GetDropResource(override val param: Resource.T) extends AbstractCommand(param) {

    override protected def verify(board: GameBoard): Mess = {
      if (command == Command.DROPRESOURCE)
        if (!deck.resou.exist(param)) return Mess(M.RESOURCENOTAVAILABLE, param)
      null
    }

    override protected def execute(board: GameBoard): Unit = {
      command match {
        case Command.DROPRESOURCE => decrResource(board, deck, param)
        case Command.GETRESOURCE => takeResourceFromBoard(board, deck, param, isExecute)
      }
    }
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new GetDropResource(param)

}

