package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard.GameBoard
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}
import play.api.libs.json.JsValue

object SpendTrade extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.SPENDTRADE, Command.UNDOSPENDTRADE)

  private def itemizeCommandsForSpendTrade(b : gameboard.GameBoard, civ:Civilization.T) : Seq[P] = {
    var trade = numberofTrade(b, civ)
    val li: PlayerLimits = getLimits(b, civ)
    if (trade.trade < li.tradeforProd) return Nil
    CityAvailableForAction(b, civ)
  }

  private def itemizeCommandsForUndoSpendTrade(b : gameboard.GameBoard, civ:Civilization.T) : Seq[P] = {
    val trSet : Set[P] = spendProdForCities(b,civ).filter(p => p._2 > 0).map(_._1) toSet
    val avSet : Set[P] = CityAvailableForAction(b, civ) toSet
    val res : Set[P] =  (trSet & avSet)
    res.toSeq
  }

  protected class SpendTrade(override val param: Integer) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val trade: TradeForCiv = numberofTrade(board, civ)
      val li: PlayerLimits = getLimits(board, civ)
      if (li.prodForTrade(param) > trade.trade) return Mess(M.NOTENOUGHTRADETOSPENDFORPROD, (command, trade.trade))
      null
    }

    // do nothing
    override def execute(board: gameboard.GameBoard): Unit = Unit
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) =
    if (command == Command.SPENDTRADE) new SpendTrade (toInt(param)) else emptyCommand()

  override def itemizePP(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[P] = {
    if (com == Command.SPENDTRADE) itemizeCommandsForSpendTrade(b,civ) else itemizeCommandsForUndoSpendTrade(b,civ)
  }
}
