package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard.{BuildingPoint, GameBoard, PlayerDeck, PlayerTechnology}
import civilization.helper.PotteryPhilosophyAction.{PotteryPhilosophyAction, findpltechnology, getListOfCities}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GameResources
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.{action, gameboard, message, objects}
import civilization.objects._
import play.api.libs.json.JsValue

object IncreaseTradeProductionAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.INCREASETRADE, Command.INCREASEPRODUCTION)

  // does nothing, only place holder
  protected class IncreaseTradeAction(override val param: Int) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = null

    override def execute(board: gameboard.GameBoard): Unit = {}
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new IncreaseTradeAction(param)

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = Nil

}
