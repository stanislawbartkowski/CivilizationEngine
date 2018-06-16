package civilization.helper

import civilization.action
import civilization.action.AbstractCommand1
import civilization.gameboard.GameBoard
import civilization.action
import civilization.message.{M, Mess}
import civilization.objects._

class EndOfResearchAction extends AbstractCommand1(TurnPhase.Research, 0) {

  override def verify(board: GameBoard): Mess = {
    val c : CurrentPhase = currentPhase(board)
    if (c.turnPhase != TurnPhase.Research) return Mess(M.CANNOTENDOFRESEARCHTWICEORNOTREASEARCH, param)
    null
  }

  override def execute(board: GameBoard): Unit = {
    // calculate trade for civ to keep in case of unconsumed research
    param1 = numberofTrade(board, civ).trade
    // additional action : rotate order of players
    val c : CurrentPhase = currentPhase(board)
    if (c.notcompleted.length == 1 && c.notcompleted.head == civ) board.rotateplorder
    if (isExecute && hasWonderFeature(board,civ,WonderFeatures.getFreeCultureStartOfTurn)) {
      // StartOfTurn
      val commandC = action.constructCommand(Command.GETCULTURE, civ, null)
      board.addForcedCommand(commandC)
    }
  }
}
