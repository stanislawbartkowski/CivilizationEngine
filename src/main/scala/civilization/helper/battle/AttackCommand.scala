package civilization.helper.battle

import civilization.action.{AbstractCommand, AbstractCommandNone, Command, constructCommand}
import civilization.gameboard._
import civilization.helper._
import civilization.io.readdir.GameResources
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message
import civilization.message.{J, M, Mess}
import civilization.objects._
import play.api.libs.json.{JsNumber, JsValue}

object AttackCommand extends ImplicitMiximToJson {

  private def removeUnits(u: Seq[CombatUnit], units: Seq[CombatUnit]): Seq[CombatUnit] = removeFromSeq(u, units, (p1: CombatUnit, p2: CombatUnit) => {
    p1 == p2
  })

  private def removePlayerUnits(board: GameBoard, pl: PlayerDeck, units: Seq[CombatUnit]) = {
    pl.units = removeUnits(pl.units, units)
  }

  private def createEmptyFighting(p: BattleStart): BattleArmy = {
    val size: Int = p.defender.length + p.attacker.length
    // TODO: probably can be done better, allocate and fill with initial values
    val a: BattleArmy = Array.ofDim(size)
    for (i <- 0 until size) a(i) = None
    a
  }

  private def createBattleSide(board: GameBoard, pl: PlayerDeck, units: Seq[CombatUnit], p: BattleStart, isScouts: Boolean, ma: Option[MapSquareP]): BattleFieldSide = {
    val li: PlayerLimits = getLimits(board, pl)
    // can use iron
    val combatBonus: Int = li.combatBonus + (if (ma.isDefined && ma.get.s.city.isDefined) ma.get.s.city.get.defenceStrength() else 0)
    BattleFieldSide(createEmptyFighting(p), units, Nil, pl.combatlevel, combatBonus, existResourceAndTech(board, pl, Resource.Iron, TechnologyName.Metallurgy), false, isScouts)
  }

  private def createBattleSideForVillages(p: BattleStart): BattleFieldSide =
    BattleFieldSide(createEmptyFighting(p), p.defender, Nil, CombatUnitStrength(), 0, false, true, false)


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

  def battlesideaftermatch(b: GameBoard, s: BattleFieldSide, p: MapSquareP, plfig: Figures, winner: Boolean): Option[Figures] = {
    var survived: Seq[CombatUnit] = finghtingtocombat(s.fighting)
    val saved: Seq[CombatUnit] = if (s.savedunit.isDefined) Seq(s.killed(s.savedunit.get)) else Nil
    b.market.killedunits = removeUnits(b.market.killedunits ++ s.killed, saved)

    // return survived units
    if (!s.isvillage) {
      val pl: PlayerDeck = b.playerDeck(p.civHere.get)
      pl.units = pl.units ++ survived ++ saved
    }
    //    if (winner) {
    //      val pl: PlayerDeck = b.playerDeck(p.civHere.get)
    //      pl.units = pl.units ++ survived ++ saved
    //    }
    // else kill them all
    //    else {
    //      b.market.killedunits = b.market.killedunits ++ survived
    //      if (!s.isvillage) {
    //        val pl: PlayerDeck = b.playerDeck(p.civHere.get)
    //        pl.units = pl.units ++ saved
    //      }
    //    }
    // figures participating in the battle
    // important: 2017/12/24
    // p.civHere.get not civ !
    // the winner can be different then attacker
    //    val pla: PlayerMove = getCurrentMove(b, p.civHere.get).get
    //    var plfig = pla.f
    var fig: Option[Figures] = None
    var f: Figures = null
    if (winner) {
      // kill figure for every two killed units leaving at least one
      val tokill = math.min(s.killed.length / 2, plfig.numberofArmies - 1)
      if (tokill == 0) f = Figures(0, 0)
      else {
        f = Figures(-tokill, 0)
        fig = Option(Figures(plfig.numberofArmies - tokill, plfig.numberofScouts))
      }
    }
    else
    // kill all figures participating in the battle
      f = Figures(-plfig.numberofArmies, -plfig.numberofScouts)

    p.s.figures + f
    fig
  }

  private def takerandomresohv(pl: PlayerDeck, hv: HutVillage.T): Resource.T = {
    val h: HutVillage.T = HutVillage.Hut
    val hvlist: Seq[HutVillage] = pl.hvlist.filter(_.hv == hv)
    return getRandom(hvlist.map(_.resource))
  }

  private def takerandomcard(pl: PlayerDeck, level: Int): CultureCardName.T = {
    val cardlist: Seq[CultureCardName.T] = pl.cultureresource.cards.filter(c => GameResources.getCultureCard(c).level == level)
    return getRandom(cardlist)
  }

  private def verifyloot(g: GameBoard, param: Seq[WinnerLootEffect]): Option[Mess] = {
    if (param.isEmpty) return None
    val l: WinnerLoot = BattleActions.winnerLoot(g)
    param.foreach(lo => {
      if (!l.list.contains(lo)) return Some(Mess(M.IMPROPERLOOTCANNOTGETTHISFROMLOSER, param))
    })
    None
  }

  private def executehv(g: GameBoard, hv: HutVillage.T, winnerciv: Civilization.T, looserciv: Civilization.T) = {

    val re: Resource.T = takerandomresohv(g.playerDeck(looserciv), hv)
    val hutVillage: HutVillage = HutVillage(hv, re)
    g.addForcedCommandC(Command.DROPHUTVILLAGE, looserciv, null, hutVillage)
    g.addForcedCommandC(Command.GETHUTVILLAGE, winnerciv, null, hutVillage)
  }


  private def executeloot(g: GameBoard, batt: BattleField, param: Seq[WinnerLootEffect], winnerciv: Civilization.T, looserciv: Civilization.T) {
    val looser: PlayerDeck = g.playerDeck(looserciv)
    param.foreach(e => {
      e.name match {
        case LootEffectName.trade => {
          val trade: Int = math.min(numberofTrade(g, looser).trade, WINNERTRADELOOT)
          g.increaseTradeCommand(winnerciv, trade)
          g.increaseTradeCommand(looserciv, -trade)
        }
        case LootEffectName.resource => {
          g.addForcedCommandC(Command.DROPRESOURCE, looserciv, null, e.resource.get)
          g.addForcedCommandC(Command.GETRESOURCE, winnerciv, null, e.resource.get)
        }
        case LootEffectName.hut => executehv(g, HutVillage.Hut, winnerciv, looserciv)
        case LootEffectName.village => executehv(g, HutVillage.Village, winnerciv, looserciv)
        case LootEffectName.culture => {
          val cult = math.min(WINNERCULTURELOOT, looser.resou.nof(Resource.Culture))
          g.increaseCultureCommand(winnerciv, cult)
          g.increaseCultureCommand(looserciv, -cult)
        }
        case LootEffectName.card => {
          val ca: CultureCardName.T = takerandomcard(looser, e.cardlevel.get)
          g.addForcedCommandC(Command.DROPCULTURECARD, looserciv, null, ca)
          if (e.level2) g.addForcedCommandC(Command.CULTURECARD, winnerciv, null, ca)
        }
        case LootEffectName.coin => {
          if (e.coinsheet.isDefined && e.coinsheet.get) g.addForcedCommandC(Command.GETCOIN, looserciv, null, JsNumber(-1))
          else g.addForcedCommandC(Command.DROPCOINFROMTECHNOLOGY, looserciv, null, e.tech.get)
          if (e.level2) g.addForcedCommandC(Command.GETCOIN, winnerciv, null, JsNumber(1))
        }
        case LootEffectName.tech => {
          g.addForcedCommandC(Command.GETTECHNOLOGY, winnerciv, null, e.tech.get)
        }
      }
    })
  }

  class EndOfBattleCommand(override val param: Seq[WinnerLootEffect]) extends AbstractCommand(param) {

    override def verify(board: GameBoard): Mess = {
      if (!board.battle.get.endofbattle)
        return Mess(M.BATTLEISNOTFINISHED)
      val res: Option[Mess] = verifyloot(board, param)
      if (res.isDefined) return res.get
      null
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
      val f: Option[Figures] = battlesideaftermatch(board, batt.attacker, att, getCurrentMove(board, attackciv).get.f.toFigures, batt.attackerwinner)
      if (defeciv.isEmpty)
      // if village all unit return to killed
        board.market.killedunits = board.market.killedunits ++ batt.defender.killed ++ finghtingtocombat(batt.defender.fighting)
      else battlesideaftermatch(board, batt.defender, defe, defe.s.figures.toFigures, !batt.attackerwinner)
      // attacker will lose figures if battle is lost
      // so get civilization before
      // move all village units to killed
      //      board.market.killedunits = board.market.killedunits ++ batt.defender.killed ++ finghtingtocombat(batt.defender.fighting)
      if (batt.attackerwinner)
        if (defe.s.hvhere) {
          // get village
          val pl: PlayerDeck = board.playerDeck(attackciv)
          // get village
          pl.hvlist = pl.hvlist :+ defe.s.hv.get
          // remove village
          defe.s.hv = None
        }
      // loot
      if (isExecute) {
        if (batt.attackerwinner) {
          if (defe.civHere.isDefined && defe.s.city.isDefined)
          // city is conquered, destroy city before moving on
            board.addForcedCommandC(Command.CITYLOST, defe.civHere.get, defe.p)

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
        if (defeciv.isDefined && isExecute)
          executeloot(board, batt, param, if (batt.attackerwinner) attackciv else defeciv.get, if (batt.attackerwinner) defeciv.get else attackciv)
      }
      // who is the winner
      // implement CodeOfLaws
      if (batt.attackerwinner) TechnologyAction.BattleWinner(board, board.playerDeck(attackciv))
      else if (defeciv.isDefined) TechnologyAction.BattleWinner(board, board.playerDeck(defeciv.get))
      // import: at the end, not before
      // only if village is taken
      if (batt.attackerwinner && defeciv.isEmpty) {
        cultureforhutvillage(board, civ, isExecute)
        // bonus for winning the village
        advanceCultureForFree(board, civ, isExecute)
      }
    }
  }

  private def moveAllUnitsToFighting(side: BattleFieldSide): Unit = {
    var to: Int = 0
    while (!side.waiting.isEmpty) {
      // take first
      val u = getRemove(side.waiting, 0)
      side.waiting = u._2
      // strenght not important
      side.fighting(to) = Some(FrontUnit(u._1, 0, 0, 0))
      to = to + 1
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
        //        val pl: PlayerDeck = board.playerDeck(if (board.battle.get.attackermove) board.battle.get.attackerciv else board.battle.get.defenderciv)
        //        pl.resou.decr(Resource.Iron)
        val civ: Civilization.T = if (board.battle.get.attackermove) board.battle.get.attackerciv else board.battle.get.defenderciv
        decrResourceHVForTech(board, deck, Resource.Iron, TechnologyName.Metallurgy)
        // mark iron
        ba.canuseiron = false
        ba.ironused = to
        addToJournal(board, civ, J.ATTACKWITHIRON, null, Some(TechnologyName.Metallurgy))
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

    private def isScoutAttacked(b: GameBoard): Boolean = {
      val ma: MapSquareP = getSquare(b, p)
      return !ma.s.figures.empty && ma.s.figures.numberofArmies == 0 && ma.s.figures.numberofScouts > 0
    }

    override def verify(board: GameBoard): Mess = null

    override def execute(board: GameBoard): Unit = {
      val attackerP: P = getCurrentMove(board, civ).get.lastp
      assert(!attackerP.empty && !p.empty)
      assert(board.battle.isEmpty)
      val isScouts = isScoutAttacked(board)
      removePlayerUnits(board, deck, param.attacker)
      val ma: MapSquareP = getSquare(board, p)
      val attacker: BattleFieldSide = createBattleSide(board, deck, param.attacker, param, false, None)
      var defender: BattleFieldSide = null
      var defenderciv: Civilization.T = null
      if (ma.s.hvhere) {
        board.market.units = removeUnits(board.market.units, param.defender)
        defender = createBattleSideForVillages(param)
        defenderciv = nextCiv(board, civ)
      }
      else {
        val defe: PlayerDeck = board.playerDeck(ma.civHere.get)
        removePlayerUnits(board, defe, param.defender)
        defender = createBattleSide(board, defe, param.defender, param, isScouts, Some(ma))
        defenderciv = ma.civHere.get
      }
      if (isScouts) {
        // move units immediately to fighting
        moveAllUnitsToFighting(attacker)
        moveAllUnitsToFighting(defender)
      }
      board.battle = Some(BattleField(attacker, defender, civ, defenderciv))
    }

  }

  class SaveUnitCommand extends AbstractCommandNone {

    private def getBattleSide(board: GameBoard): BattleFieldSide =
      if (civ == board.battle.get.attackerciv) board.battle.get.attacker
      else board.battle.get.defender

    override def verify(board: GameBoard): Mess = {
      assert(board.battle.isDefined)
      val battle: BattleField = board.battle.get
      if (!battle.endofbattle)
        return Mess(M.BATTLEISNOTFINISHED)
      if (!CivilizationFeatures.canSaveUnit(civ)) return Mess(M.YOUARENOTENTITLETOSAVEUNIT)
      val side: BattleFieldSide = getBattleSide(board)
      if (side.isvillage) return Mess(M.VILLAGEISNOTENTITLEDFORUNITTAKING)
      if (side.savedunit.isDefined) return Mess(M.YOUALREADYSAVEUNIT)
      val pos = p.row
      if (pos < 0 || pos >= side.killed.length)
        return Mess(M.SAVEDUNITINDEXOUTOFRANGE)
      null
    }

    override def execute(board: GameBoard): Unit = {
      val pos = p.row
      val side: BattleFieldSide = getBattleSide(board)
      side.savedunit = Some(pos)
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

    override def execute(board: GameBoard): Unit = {
      if (isExecute) {
        val param: BattleStart = BattleActions.assemblyCombatForces(board, civ, p)
        board.addForcedCommandC(Command.STARTBATTLE, civ, p, param)
      }
    }
  }

}
