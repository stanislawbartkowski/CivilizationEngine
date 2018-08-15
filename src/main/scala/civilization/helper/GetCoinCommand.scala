package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import play.api.libs.json.JsValue

object GetCoinCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.DROPCOINFROMTECHNOLOGY, Command.GETCOIN)

  protected class DropCoinTechnology(override val param: TechnologyName.T) extends AbstractCommand(param) {

    override protected def execute(board: GameBoard): Unit = {
      deck.tech.find(_.tech == param).get.removeCoin()
    }
  }

  protected class GetCoin(override val param: Int) extends AbstractCommand(param) {

    override protected def execute(board: GameBoard): Unit = {
      deck.resou.incr(Resource.Coin, param)
      checkEconomyVictory(board, deck, isExecute)
    }
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command =
    command match {
      case Command.DROPCOINFROMTECHNOLOGY => new DropCoinTechnology(param)
      case Command.GETCOIN => new GetCoin(param)
    }

}

