package civilization.helper

import civilization.action.AbstractCommand
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}

object ExploreHutCommand {

  class ExploreHutCommand extends AbstractCommand {

    override def verify(b: gameboard.GameBoard): message.Mess = {
      val figo: Option[PlayerMove] = getCurrentMove(b, civ)
      if (figo.isEmpty) return Mess(M.CANNOTFINDSTARTOFMOVE, p)
      val ma : MapSquareP = getSquare(b,p)
      if (!ma.s.hvhere || ma.s.hv.get.hv != HutVillage.Hut)
        return Mess(M.THEREISNOHUTATRHISPOINT,p)
      val lim: PlayerLimits = getLimits(b, civ)
      if (figo.get.f.numberofArmies == 0 && !lim.scoutscanExplore)
        return Mess(M.SCOUTCANNOTEXPLOREHUT,p)
      return null
    }

    override def execute(board: gameboard.GameBoard): Unit = exploreHutOrVillage(board,civ,p)
  }


}
