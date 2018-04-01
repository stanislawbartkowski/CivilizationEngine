package civilization.helper

import civilization.action.AbstractCommand1
import civilization.gameboard.GameBoard
import civilization.message.{M, Mess}
import civilization.{gameboard, message}
import civilization.objects.TurnPhase

class EndOfTradeAction extends AbstractCommand1(TurnPhase.Trade, 0) {

  override def verify(board: GameBoard): Mess = {
    val c : CurrentPhase = currentPhase(board)
    if (c.turnPhase != TurnPhase.Trade) return Mess(M.CANNOTENDOFTRADETWICE, param)
    null
  }


  override def execute(board: gameboard.GameBoard): Unit = {
    // keep the trade at the end of Trade phase
    param1 = numberofTradeCalculate(board, civ).trade
  }
}
