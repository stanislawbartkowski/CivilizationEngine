package civilization.helper

import civilization.action.AbstractCommand1
import civilization.gameboard._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._


abstract class EndOfPhaseAbstract(param: TurnPhase.T) extends AbstractCommand1(param, 0) with ImplicitMiximFromJson with ImplicitMiximToJson {

  protected def executeC(board: GameBoard): Unit

  private def nextResearchPlayer(board: GameBoard): Option[Civilization.T] = {
    val c: CurrentPhase = currentPhase(board)
    param match {
      case TurnPhase.Movement =>
        // end of Movement, next players is Research and pllist
        if (c.notcompleted.length == 1) Some(board.pllist.head) else None
      case TurnPhase.Research =>
        if (c.notcompleted.length == 1) None // next is start of turn
        else Some(board.pllist.tail.head)
      case _ => None
    }
  }

  override def execute(board: GameBoard): Unit = {
    if (isExecute) {
      // check technology victory
      val nextr: Option[Civilization.T] = nextResearchPlayer(board)
      if (nextr.isDefined) {
        val pl: PlayerDeck = board.playerDeck(nextr.get)
        if (techologyLevel(board, pl) contains TECHNOLOGYVICTORYLEVEL)
          board.addForcedCommandC(Command.PLAYERWIN, pl, null, GameWinType.Technology)
      }
    }
    executeC(board)
  }

  override def verify(board: GameBoard): Mess = {
    val c: CurrentPhase = currentPhase(board)
    if (c.turnPhase != param)
      return Mess(M.ENDOFPHASEALLOWEDFORONLY, (c.turnPhase,param, deck.civ,c.roundno))
    if (!(c.notcompleted contains civ))
      return Mess(M.THEPLAYERALREADYCOMPLETEDPHASE, (param, deck.civ,c.roundno))
    if (TurnPhase.turnAction(param) && c.notcompleted.head != deck.civ)
      return Mess(M.NOTPLAYERTURNTOENDOFPHASE, (param, deck.civ,c.roundno))
    null
  }

}
