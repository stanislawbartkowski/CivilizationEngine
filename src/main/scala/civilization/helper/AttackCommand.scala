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
//    val size : Int = math.max(p.defender.length, p.attacker.length)
    val size : Int = p.defender.length + p.attacker.length
    // TODO: probably can be done better, allocate and fill with initial values
    val a : BattleArmy = Array.ofDim(size)
    for (i <- 0 until size) a(i) = None
    a
  }

  private def createBattleSide(board: GameBoard, civ: Civilization.T, units: Seq[CombatUnit],p : BattleStart): BattleFieldSide = {
    val li : PlayerLimits = getLimits(board,civ)
    val pl : PlayerDeck = board.playerDeck(civ)
    // can use iron
    BattleFieldSide(createEmptyFighting(p),units,Nil,pl.combatlevel,li.combatBonus,pl.resou.nof(Resource.Iron) > 0)
  }

  private def createBattleSideForVillages(p : BattleStart): BattleFieldSide =
    BattleFieldSide(createEmptyFighting(p),p.defender,Nil,CombatUnitStrength(),0,false)


  /** Get next civilization after civ */
  private def nextCiv(b : GameBoard, civ: Civilization.T) : Civilization.T = {
    val c : Seq[Civilization.T] = allCivs(b)
    val pos : Int = c.indexOf(civ)
    // if there is only one civilization, return that civilization
    if (pos == c.length -1) c.head
    else
      c.drop(pos + 1).head
  }

  class PlayUnitCommand(val iron : Boolean) extends AbstractCommandNone {

    override def verify(board: GameBoard): Mess = {
      val ba : BattleFieldSide = if (board.battle.get.attackermove) board.battle.get.attacker else board.battle.get.defender
      if (iron && !ba.canuseiron)
        return Mess(M.CANNOTUSERIRONNOW,p)
      val from : Int = p.row
      val to : Int = p.col
      if (from >= ba.waiting.length) return Mess(M.IMPROPERUNITNUMBERTOPLAY,(p,ba.waiting.length))
      if (to >= ba.fighting.length)
        return Mess(M.IMPROPERNUMBERTOPLAYUNIT,(p,ba.fighting.length))
      if (ba.fighting(to).isDefined)
        return Mess(M.THEREISANOTHERUNITPLAYEDHERE,(p,ba.fighting(to)))
      null
    }

    private def applywound(a1 : FrontUnit, a2 : FrontUnit, trumpover : Boolean): Unit = {
      a2.wounds = a2.wounds + a1.attackstrength
      if (!trumpover || a2.wounds < a2.defendstrenght)
        a1.wounds = a1.wounds + a2.attackstrength
    }

    private def checkwounds(a : FrontUnit, aa : BattleFieldSide, pos : Int) = {
      if (a.wounds >= a.defendstrenght) {
        aa.fighting(pos) = None
        aa.killed = aa.killed :+ a.unit
      }
    }

    private def fight(attacker : BattleFieldSide, defender : BattleFieldSide, pos : Int): Unit = {
      // opposite empty, new front
      if (defender.fighting(pos).isEmpty) return
      val a : FrontUnit = attacker.fighting(pos).get
      val d : FrontUnit = defender.fighting(pos).get
      if (a.unit.utype == d.unit.utype) applywound(a,d,false) else
      if (CombatUnitType.trumpover(a.unit.utype,d.unit.utype)) applywound(a,d,true) else applywound(d,a,true)
      if (a.wounds >= a.defendstrenght) {
        attacker.fighting(pos) = None
        attacker.killed = attacker.killed :+ a.unit
      }
      checkwounds(a,attacker,pos)
      checkwounds(d,defender,pos)
    }

    override def execute(board: GameBoard): Unit = {
      val ba : BattleFieldSide = if (board.battle.get.attackermove) board.battle.get.attacker else board.battle.get.defender
      val opp : BattleFieldSide = if (board.battle.get.attackermove) board.battle.get.defender else board.battle.get.attacker
      val from : Int = p.row
      val to : Int = p.col
      // remove unit to play from waiting
      val u = getRemove(ba.waiting,from)
      val pUnit : CombatUnit = u._1
      ba.waiting = u._2
      // create front unit
      var attackstrength = pUnit.strength(ba.strength.getStrength(pUnit.utype))
      var defendstrength = attackstrength
      if (iron) {
        attackstrength = attackstrength + IRONSTRENGTH
        // remove iron
        val pl : PlayerDeck = board.playerDeck(if (board.battle.get.attackermove) board.battle.get.attackerciv else board.battle.get.defenderciv)
        pl.resou.decr(Resource.Iron)
        // mark iron
        ba.canuseiron = false
        ba.ironused = to
      }
      if (board.conf.ironincreasedefend) defendstrength = attackstrength
      ba.fighting(to) = Some(FrontUnit(pUnit,attackstrength,defendstrength,0))
      fight(ba,opp,to)
      // there are units to play
      if (!ba.waiting.isEmpty && !opp.waiting.isEmpty) {
        // next player
        board.battle.get.attackermove = !board.battle.get.attackermove
        return
      }
      // movevent of player having units to play
      board.battle.get.attackermove = opp.waiting.isEmpty
    }
  }

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
      var defenderciv : Civilization.T = null
      if (ma.s.hvhere) {
        board.market.units = removeUnits(board.market.units,param.defender)
        defender = createBattleSideForVillages(param)
        defenderciv = nextCiv(board,civ)
      }
      else {
        removePlayerUnits(board,ma.civHere.get,param.defender)
        defender = createBattleSide(board,ma.civHere.get,param.defender,param)
        defenderciv = ma.civHere.get
      }

      board.battle = Some(BattleField(attacker,defender,civ,defenderciv))
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
