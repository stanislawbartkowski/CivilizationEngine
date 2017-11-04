package civilization.helper

import civilization.action.{AbstractCommand, AbstractCommandNone, Command, constructCommand}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}

import scala.util.control.Breaks.{breakable, _}

object AttackCommand extends ImplicitMiximToJson {

    private def removeUnits(u : Seq[CombatUnit], units: BattleArmy) : Seq[CombatUnit] = {
      val b = units.army.toBuffer
      u.filter(p => {
        var res: Boolean = true
        breakable {
          for (i <- 0 until b.size)
            if (b(i).unit.get == p) {
              b.remove(i)
              res = false
              break
            }
        }
        res
      }
      )
    }

    private def removePlayerUnits(board: gameboard.GameBoard, civ: Civilization.T, units: BattleArmy) = {
      val pl: gameboard.PlayerDeck = board.playerDeck(civ)
      pl.units = removeUnits(pl.units, units)
    }

  class StartBattleCommand(override val param: BattleStart) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): Mess = null

    override def execute(board: gameboard.GameBoard): Unit = {
      val attackerP: P = getCurrentMove(board, civ).get.lastp
      assert(!attackerP.empty && !p.empty)
      removePlayerUnits(board,civ,param.attacker)
      val ma: MapSquareP = getSquare(board, p)
      if (ma.s.hvhere) board.market.units = removeUnits(board.market.units,param.defender)
      else removePlayerUnits(board,ma.civHere.get,param.defender)
    }
  }


  class AttackCommand extends AbstractCommandNone {

    override def verify(b: gameboard.GameBoard): message.Mess = {
      val figo: Option[PlayerMove] = getCurrentMove(b, civ)
      if (figo.isEmpty) return Mess(M.CANNOTFINDSTARTOFMOVE, p)
      if (figo.get.f.numberofArmies == 0)
        return Mess(M.CANNOTFINDFIGUREONATTACKINGSQUARE, p)

      val ma: MapSquareP = getSquare(b, p)
      if (ma.s.figures.civOccupying(civ))
        return Mess(M.CANNOTATTACKOWNFIGURE, p)
      if (ma.s.cityhere && ma.s.city.get.belongsTo(civ))
        return Mess(M.CANNOTATTACKOWNCITY, p)
      var ok: Boolean = false
      if (ma.s.hv.isDefined && ma.s.hv.get.hv == HutVillage.Village) ok = true
      if (!ma.s.figures.empty) ok = true
      if (ma.s.cityhere) ok = true
      if (!ok) return Mess(M.CANNOTFINFANYTHINGONTHEQUARETOATTACK, p)
      return null
    }

    private def killScouts(board: gameboard.GameBoard): Boolean = {
      val ma: MapSquareP = getSquare(board, p)
      if (!ma.s.figures.empty && ma.s.figures.numberofArmies == 0 && ma.s.figures.numberofScouts > 0) {
        ma.s.figures.kill()
        moveFigures(board, civ, p)
        return true
      }
      false
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      if (killScouts(board)) return
      if (isExecute) {
        val param: BattleStart = BattleActions.assemblyCombatForces(board, civ, p)
        val command: Command = constructCommand(Command.STARTBATTLE, civ, p, param)
        // execute later
        board.addForcedCommand(command)
      }

    }

  }

}
