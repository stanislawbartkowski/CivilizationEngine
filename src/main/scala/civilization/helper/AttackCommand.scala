package civilization.helper

import civilization.action.{AbstractCommand, AbstractCommandNone, Command, constructCommand}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{FatalError, M, Mess}
import civilization.objects._
import civilization.gameboard._
import civilization.message
import play.api.libs.json.JsValue

import scala.util.control.Breaks.{breakable, _}

object AttackCommand extends ImplicitMiximToJson {

  private def removeUnits(u: Seq[CombatUnit], units: Seq[CombatUnit]): Seq[CombatUnit] = removeFromSeq(u, units, (p1: CombatUnit, p2: CombatUnit) => {
    p1 == p2
  })

  private def removePlayerUnits(board: GameBoard, civ: Civilization.T, units: Seq[CombatUnit]) = {
    val pl: PlayerDeck = board.playerDeck(civ)
    pl.units = removeUnits(pl.units, units)
  }

  private def createEmptyFighting(p: BattleStart): BattleArmy = {
    //    val size : Int = math.max(p.defender.length, p.attacker.length)
    val size: Int = p.defender.length + p.attacker.length
    // TODO: probably can be done better, allocate and fill with initial values
    val a: BattleArmy = Array.ofDim(size)
    for (i <- 0 until size) a(i) = None
    a
  }

  private def createBattleSide(board: GameBoard, civ: Civilization.T, units: Seq[CombatUnit], p: BattleStart): BattleFieldSide = {
    val li: PlayerLimits = getLimits(board, civ)
    val pl: PlayerDeck = board.playerDeck(civ)
    // can use iron
    BattleFieldSide(createEmptyFighting(p), units, Nil, pl.combatlevel, li.combatBonus, pl.resou.nof(Resource.Iron) > 0, false)
  }

  private def createBattleSideForVillages(p: BattleStart): BattleFieldSide =
    BattleFieldSide(createEmptyFighting(p), p.defender, Nil, CombatUnitStrength(), 0, false, true)


  /** Get next civilization after civ */
  private def nextCiv(b: GameBoard, civ: Civilization.T): Civilization.T = {
    val c: Seq[Civilization.T] = allCivs(b)
    val pos: Int = c.indexOf(civ)
    // if there is only one civilization, return that civilization
    if (pos == c.length - 1) c.head
    else
      c.drop(pos + 1).head
  }

  // problem with implicit
  private def finghtingtocombat(aa: BattleArmy): Seq[CombatUnit] =
    aa.filter(f => f.isDefined).map(_.get.unit)

  def battlesideaftermatch(b: GameBoard, s: BattleFieldSide, p: MapSquareP, winner: Boolean): Option[Figures] = {
    b.market.killedunits = b.market.killedunits ++ s.killed
    val civattacker: Civilization.T = p.civHere.get
    if (s.ironused > -1) b.resources.resou.incr(Resource.Iron)
    val survived: Seq[CombatUnit] = finghtingtocombat(s.fighting)
    val pl: PlayerDeck = b.playerDeck(civattacker)
    // return survived units
    if (winner) pl.units = pl.units ++ survived
    // else kill them all
    else b.market.killedunits = b.market.killedunits ++ survived
    // fugures participating in the battle
    // important: 2017/12/24
    // p.civHere.get not civ !
    // the winner can be different then attacker
    val pla: PlayerMove = getCurrentMove(b, civattacker).get
    var fig: Option[Figures] = None
    var f: Figures = null
    if (winner) {
      // kill figure for every two killed units leaving at least one
      val tokill = Math.min(s.killed.length / 2, pla.f.numberofArmies - 1)
      if (tokill == 0) f = Figures(0, 0)
      else {
        f = Figures(-tokill, 0)
        fig = Option(Figures(pla.f.numberofArmies - tokill, pla.f.numberofScouts))
      }
    }
    else
    // kill all figures participating in the battle
      f = Figures(-pla.f.numberofArmies, -pla.f.numberofScouts)

    p.s.figures + f
    fig
  }

  private def takerandomresohv(pl: PlayerDeck, hv: HutVillage.T): Resource.T = {
    val h: HutVillage.T = HutVillage.Hut
    val hvlist: Seq[HutVillage] = pl.hvlist.filter(_.hv == hv)
    return getRandom(hvlist.map(_.resource))
  }

  private def prepareloot(g: GameBoard, batt: BattleField, param: WinnerLoot, attackciv: Civilization.T, defeciv: Civilization.T) {
    if (param.noloot()) return
    // there is a loot
    val winnerciv: Civilization.T = if (batt.attackerwinner) attackciv else defeciv
    val loserciv: Civilization.T = if (batt.attackerwinner) defeciv else attackciv

    val reso: Option[Resource.T] =
      if (param.hv.isDefined) Some(takerandomresohv(g.playerDeck(loserciv), param.hv.get)) else if (param.res.isDefined) param.res else None

    var trade: Integer = 0
    if (param.trade) {
      val tradeloser = numberofTrade(g, loserciv).trade
      trade = Math.min(MAXLOOTTRADE, tradeloser)
    }
    val tlook: TakeWinnerLoot = TakeWinnerLoot(winnerciv, loserciv, param, reso, trade)
    val command: Command = constructCommand(Command.TAKEWINNERLOOT, attackciv, null, tlook)
    g.addForcedCommand(command)
  }

  private def verifyloot(g: GameBoard, param: WinnerLoot): Option[Mess] = {
    if (param.noloot()) return None
    val l: Seq[WinnerLoot] = BattleActions.winnerLoot(g)
    if (l.find(_ == param).isDefined) return None
    Some(Mess(M.IMPROPERLOOTCANNOTGETTHISFROMLOSER, param))
  }

  class TakeWinnerLootCommand(override val param: TakeWinnerLoot) extends AbstractCommand(param) {

    override def verify(board: GameBoard): Mess = null

    override def execute(board: GameBoard): Unit = {
      val pl: PlayerDeck = board.playerDeck(param.winner)
      val los: PlayerDeck = board.playerDeck(param.loser)
      if (param.loot.hv.isDefined) {
        val hv: HutVillage = HutVillage(param.loot.hv.get, param.reso.get)
        // give winner
        pl.hvlist = pl.hvlist :+ hv
        // take from loser
        val fun: (HutVillage, HutVillage) => Boolean = (p1: HutVillage, p2: HutVillage) => {
          p1 == p2
        }
        los.hvlist = removeElem(los.hvlist, hv, fun)
      }
      if (param.loot.res.isDefined) {
        // move resource
        // take loser
        los.resou.decr(param.reso.get)
        // give winner
        pl.resou.incr(param.reso.get)
      }
    }
  }


  class EndOfBattleCommand(override val param: WinnerLoot) extends AbstractCommand(param) {

    override def verify(board: GameBoard): Mess = {
      if (!board.battle.get.endofbattle)
        return Mess(M.BATTLEISNOTFINISHED)
      val res: Option[Mess] = verifyloot(board, param)
      if (res.isDefined) return res.get
      return null
    }

    override def execute(board: GameBoard): Unit = {
      val ba: (P, P) = battleParticipants(board)
      val att: MapSquareP = getSquare(board, ba._1)
      // import, take attackign civilization before battle conclusion
      val attackciv: Civilization.T = att.civHere.get
      val defe: MapSquareP = getSquare(board, ba._2)
      val defeciv: Option[Civilization.T] = defe.civHere
      val batt: BattleField = board.battle.get
      // close battle
      board.battle = None
      // attacker aftermath
      val f: Option[Figures] = battlesideaftermatch(board, batt.attacker, att, batt.attackerwinner)
      // attacker will lose figures if battle is lost
      // so get civilization before
      //      if (!defe.s.hvhere)
      //        throw new FatalError(Mess(M.ENDOFBATTLEIMPLEMENTEDONLYFORVILLAGES, defe.p))
      // move all village units to killed
      board.market.killedunits = board.market.killedunits ++ batt.defender.killed ++ finghtingtocombat(batt.defender.fighting)
      if (batt.attackerwinner)
        if (defe.s.hvhere) {
          // get village
          val pl: PlayerDeck = board.playerDeck(attackciv)
          // get village
          pl.hvlist = pl.hvlist :+ defe.s.hv.get
          // remove village
          defe.s.hv = None
        }
        else {
          // kill enemy figures
          defe.s.figures.kill()
        }
      // loot

      if (isExecute) {
        if (batt.attackerwinner) {
          // move army to this point
          val param: JsValue = if (f.isEmpty) null else f.get
          val command: Command = constructCommand(Command.ENDOFMOVE, attackciv, defe.p, param)
          // execute later
          board.addForcedCommand(command)
        }
        else {
          // battle lost, nothing
          val command: Command = constructCommand(Command.ENDOFMOVE, attackciv, null, null)
          board.addForcedCommand(command)
        }
        if (defeciv.isDefined)
          prepareloot(board, batt, param, attackciv, defeciv.get)
      }
    }
  }

  class PlayUnitCommand(val iron: Boolean) extends AbstractCommandNone {

    override def verify(board: GameBoard): Mess = {
      if (board.battle.get.endofbattle)
        return Mess(M.ENDOFBATTLEALREADY, p)
      val ba: BattleFieldSide = if (board.battle.get.attackermove) board.battle.get.attacker else board.battle.get.defender
      if (iron && !ba.canuseiron)
        return Mess(M.CANNOTUSERIRONNOW, p)
      val from: Int = p.row
      val to: Int = p.col
      if (from >= ba.waiting.length) return Mess(M.IMPROPERUNITNUMBERTOPLAY, (p, ba.waiting.length))
      if (to >= ba.fighting.length)
        return Mess(M.IMPROPERNUMBERTOPLAYUNIT, (p, ba.fighting.length))
      if (ba.fighting(to).isDefined)
        return Mess(M.THEREISANOTHERUNITPLAYEDHERE, (p, ba.fighting(to)))
      null
    }

    private def applywound(a1: FrontUnit, a2: FrontUnit, trumpover: Boolean): Unit = {
      a2.wounds = a2.wounds + a1.attackstrength
      if (!trumpover || a2.wounds < a2.defendstrenght)
        a1.wounds = a1.wounds + a2.attackstrength
    }

    private def checkwounds(aa: BattleFieldSide, pos: Int) = {
      val a: FrontUnit = aa.fighting(pos).get
      if (a.wounds >= a.defendstrenght) {
        aa.fighting(pos) = None
        aa.killed = aa.killed :+ a.unit
      }
    }

    private def fight(attacker: BattleFieldSide, defender: BattleFieldSide, pos: Int): Unit = {
      // opposite empty, new front
      if (defender.fighting(pos).isEmpty) return
      val a: FrontUnit = attacker.fighting(pos).get
      val d: FrontUnit = defender.fighting(pos).get
      if (a.unit.utype == d.unit.utype) applywound(a, d, false) else if (CombatUnitType.trumpover(a.unit.utype, d.unit.utype)) applywound(a, d, true) else applywound(d, a, true)
      checkwounds(attacker, pos)
      checkwounds(defender, pos)
    }

    override def execute(board: GameBoard): Unit = {
      val ba: BattleFieldSide = if (board.battle.get.attackermove) board.battle.get.attacker else board.battle.get.defender
      val opp: BattleFieldSide = if (board.battle.get.attackermove) board.battle.get.defender else board.battle.get.attacker
      val from: Int = p.row
      val to: Int = p.col
      // remove unit to play from waiting
      val u = getRemove(ba.waiting, from)
      val pUnit: CombatUnit = u._1
      ba.waiting = u._2
      // create front unit
      var attackstrength = pUnit.getStrength(ba.strength)
      var defendstrength = attackstrength
      if (iron) {
        attackstrength = attackstrength + IRONSTRENGTH
        // remove iron
        val pl: PlayerDeck = board.playerDeck(if (board.battle.get.attackermove) board.battle.get.attackerciv else board.battle.get.defenderciv)
        pl.resou.decr(Resource.Iron)
        // mark iron
        ba.canuseiron = false
        ba.ironused = to
      }
      if (board.conf.ironincreasedefend) defendstrength = attackstrength
      ba.fighting(to) = Some(FrontUnit(pUnit, attackstrength, defendstrength, 0))
      fight(ba, opp, to)
      // there are units to play
      if (!ba.waiting.isEmpty && !opp.waiting.isEmpty) {
        // next player
        board.battle.get.attackermove = !board.battle.get.attackermove
        return
      }
      // movevent of player having units to play
      board.battle.get.attackermove = board.battle.get.defender.waiting.isEmpty
    }
  }

  class StartBattleCommand(override val param: BattleStart) extends AbstractCommand(param) {

    override def verify(board: GameBoard): Mess = null

    override def execute(board: GameBoard): Unit = {
      val attackerP: P = getCurrentMove(board, civ).get.lastp
      assert(!attackerP.empty && !p.empty)
      assert(board.battle.isEmpty)
      removePlayerUnits(board, civ, param.attacker)
      val ma: MapSquareP = getSquare(board, p)
      val attacker: BattleFieldSide = createBattleSide(board, civ, param.attacker, param)
      var defender: BattleFieldSide = null
      var defenderciv: Civilization.T = null
      if (ma.s.hvhere) {
        board.market.units = removeUnits(board.market.units, param.defender)
        defender = createBattleSideForVillages(param)
        defenderciv = nextCiv(board, civ)
      }
      else {
        removePlayerUnits(board, ma.civHere.get, param.defender)
        defender = createBattleSide(board, ma.civHere.get, param.defender, param)
        defenderciv = ma.civHere.get
      }

      board.battle = Some(BattleField(attacker, defender, civ, defenderciv))
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
        moveFigures(board, civ, p, None)
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
