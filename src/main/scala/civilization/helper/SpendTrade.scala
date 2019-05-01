package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}
import play.api.libs.json.JsValue

object SpendTrade extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.SPENDTRADE, Command.UNDOSPENDTRADE)

  private def itemizeCommandsForSpendTrade(b: gameboard.GameBoard, deck: PlayerDeck): Seq[P] = {
    var trade = numberofTrade(b, deck)
    val li: PlayerLimits = getLimits(b, deck)
    if (trade.trade < li.tradeforProd) return Nil
    CityAvailableForAction(b, deck)
  }

  private def itemizeCommandsForUndoSpendTrade(b: gameboard.GameBoard, civ: Civilization.T): Seq[P] = {
    val trSet: Set[P] = spendProdForCities(b, civ).filter(p => p._2 > 0).map(_._1) toSet
    val avSet: Set[P] = CityAvailableForAction(b, civ) toSet
    val res: Set[P] = (trSet & avSet)
    res.toSeq
  }

  protected class SpendTrade(override val param: Int) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val trade: TradeForCiv = numberofTrade(board, deck)
      val li: PlayerLimits = getLimits(board, deck)
      if (li.prodtoTrade(param) > trade.trade) return Mess(M.NOTENOUGHTRADETOSPENDFORPROD, (command, trade.trade))
      null
    }

    // do nothing
    override def execute(board: gameboard.GameBoard): Unit = Unit
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) =
    command match {
      case Command.SPENDTRADE => new SpendTrade(param)
      case Command.UNDOSPENDTRADE => emptyCommand()
    }

  override def itemizePP(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[P] =
    com match {
      case Command.SPENDTRADE => itemizeCommandsForSpendTrade(b, deck)
      case Command.UNDOSPENDTRADE => itemizeCommandsForUndoSpendTrade(b, deck.civ)
  }
}
