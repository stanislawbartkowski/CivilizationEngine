package civilization.helper

import civilization.action.{AbstractCommand, AbstractCommandNone, Command, constructCommand}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess, FatalError}
import civilization.objects._
import civilization.gameboard._
import civilization.message

import scala.util.control.Breaks.{breakable, _}

object AttackCommand extends ImplicitMiximToJson {

    private def removeUnits(u : Seq[CombatUnit], units: Seq[CombatUnit]) : Seq[CombatUnit] = {
      val b = units.toBuffer
      val uu = u.filter(p => {
        var res: Boolean = true
        breakable {
          for (i <- 0 until b.size)
            if (b(i) == p) {
              b.remove(i)
              res = false
              break
            }
        }
        res
      }
      )
      if (!b.isEmpty)
        throw new FatalError(Mess(M.CANNOTREMOVEUNITS,units))
      uu
    }

    private def removePlayerUnits(board: GameBoard, civ: Civilization.T, units: Seq[CombatUnit]) = {
      val pl: PlayerDeck = board.playerDeck(civ)
      pl.units = removeUnits(pl.units, units)
    }

  private def createEmptyFighting(p : BattleStart) : BattleArmy = {
    val size : Int = math.max(p.defender.length, p.attacker.length)
    // TODO: probably can be done better, allocate and fill with initial values
    val a : BattleArmy = Array.ofDim(size)
    for (i <- 0 until size) a(i) = None
    a
  }

  private def createBattleSide(board: GameBoard, civ: Civilization.T, units: Seq[CombatUnit],p : BattleStart): BattleFieldSide = {
    val li : PlayerLimits = getLimits(board,civ)
    val pl : PlayerDeck = board.playerDeck(civ)
    BattleFieldSide(createEmptyFighting(p),units,Nil,pl.combatlevel,li.combatBonus)
  }

  private def createBattleSideForVillages(p : BattleStart): BattleFieldSide =
    BattleFieldSide(createEmptyFighting(p),p.defender,Nil,CombatUnitStrength(),0)

  class StartBattleCommand(override val param: BattleStart) extends AbstractCommand(param) {

    override def verify(board: GameBoard): Mess = null

    override def execute(board: GameBoard): Unit = {
      val attackerP: P = getCurrentMove(board, civ).get.lastp
      assert(!attackerP.empty && !p.empty)
      assert(board.battle.isEmpty)
      removePlayerUnits(board,civ,param.attacker)
      val ma: MapSquareP = getSquare(board, p)
      val attacker : BattleFieldSide = createBattleSide(board,civ,param.attacker,param)
      var defender : BattleFieldSide = null
      if (ma.s.hvhere) {
        board.market.units = removeUnits(board.market.units,param.defender)
        defender = createBattleSideForVillages(param)
      }
      else {
        removePlayerUnits(board,ma.civHere.get,param.defender)
        defender = createBattleSide(board,ma.civHere.get,param.defender,param)
      }
      board.battle = Some(BattleField(attacker,defender))
    }

  }


  class AttackCommand extends AbstractCommandNone {

    override def verify(b: GameBoard): message.Mess = {
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

    private def killScouts(board: GameBoard): Boolean = {
      val ma: MapSquareP = getSquare(board, p)
      if (!ma.s.figures.empty && ma.s.figures.numberofArmies == 0 && ma.s.figures.numberofScouts > 0) {
        ma.s.figures.kill()
        moveFigures(board, civ, p)
        return true
      }
      false
    }

    override def execute(board: GameBoard): Unit = {
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
