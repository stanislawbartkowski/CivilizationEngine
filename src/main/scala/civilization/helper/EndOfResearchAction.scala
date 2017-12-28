package civilization.helper

import civilization.action.AbstractCommand1
import civilization.gameboard.GameBoard
import civilization.message.Mess
import civilization.objects._

class EndOfResearchAction extends AbstractCommand1(TurnPhase.Research, 0) {

  override def verify(board: GameBoard): Mess = null

  override def execute(board: GameBoard): Unit = {
    // calculate trade for civ to keep in case of unconsumed research
    param1 = numberofTrade(board, civ).trade
    // additional action : rotate order of players
    val c : CurrentPhase = currentPhase(board)
    val cc = 2
    if (c.notcompleted.length == 1 && c.notcompleted.head == civ) board.rotateplorder
  }
}
