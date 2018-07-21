package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.{gameboard, message}
import civilization.objects.{Civilization, Command, P, Resource}
import play.api.libs.json.JsValue


object GetCultureAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.GETCULTURE)

  protected class IncreaseCultureAction(override val param: Int) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = null

    override def execute(board: gameboard.GameBoard): Unit =
      deck.resou.incr(Resource.Culture, param)
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new IncreaseCultureAction(param)

}