package civilization.helper

import civilization.action.AbstractCommand
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}

object SpendTrade {

  def itemizeCommandsForSpendTrade(b : gameboard.GameBoard, civ:Civilization.T) : Seq[P] = {
    var trade = numberofTrade(b, civ)
    if (trade.trade < TRADEFORPROD) return Nil
    CityAvailableForAction(b, civ)
  }

  def itemizeCommandsForUndoSpendTrade(b : gameboard.GameBoard, civ:Civilization.T) : Seq[P] = {
    val trSet : Set[P] = spendProdForCities(b,civ).filter(p => p._2 > 0).map(_._1) toSet
    val avSet : Set[P] = CityAvailableForAction(b, civ) toSet
    val res : Set[P] =  (trSet & avSet)
    res.toSeq
  }

  class SpendTrade(override val param: Integer) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val trade: TradeForCiv = numberofTrade(board, civ)
      if (prodForTrade(param) > trade.trade) return Mess(M.NOTENOUGHTRADETOSPENDFORPROD, (command, trade.trade))
      null
    }

    // do nothing
    override def execute(board: gameboard.GameBoard): Unit = Unit
  }


}
