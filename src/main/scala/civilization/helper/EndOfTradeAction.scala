package civilization.helper

import civilization.gameboard
import civilization.objects.TurnPhase

class EndOfTradeAction extends EndOfPhaseAbstract(TurnPhase.Trade) {

  override protected def executeC(board: gameboard.GameBoard): Unit = {
    // keep the trade at the end of Trade phase
    param1 = numberofTradeCalculate(board, deck).trade
  }
}
