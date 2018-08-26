package civilization.helper

import civilization.action.{AbstractCommandNone, CommandPackage}
import civilization.gameboard._
import civilization.helper.DestroyCityAction.DestroyCityCommand
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.{action, gameboard, message}
import civilization.message.{M, Mess}
import civilization.objects.Civilization.T
import civilization.objects.Command.T
import civilization.objects._
import play.api.libs.json.{JsNumber, JsValue}

object DemocracyAction extends CoinActionTrait {

  override val command: Command.T = Command.DEMOCRACYACTION

  override def validateH(b: GameBoard, deck: PlayerDeck, command: Command.T): Option[Mess] = {
    val trade: TradeForCiv = numberofTrade(b, deck)
    if (trade.trade < 6)
      Some(Mess(M.NOTENOUGHTRADETOSPEND, (command, trade)))
    else None
  }

  override def executeI(board: gameboard.GameBoard, deck : PlayerDeck): Unit =
    board.increaseTradeCommand(deck,-6)

}
