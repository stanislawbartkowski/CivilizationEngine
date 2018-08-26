package civilization.helper

import civilization.action.{AbstractCommandNone, CommandPackage}
import civilization.gameboard.{GameBoard, _}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message
import civilization.message.{M, Mess}
import civilization.objects.{Civilization, Command, CultureCardName, P}
import play.api.libs.json.JsValue

object DestroyCityAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.CITYLOST)

  class DestroyCityCommand extends AbstractCommandNone {

    override def execute(board: GameBoard): Unit = destroyCity(board,deck,p)
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new DestroyCityCommand

}
