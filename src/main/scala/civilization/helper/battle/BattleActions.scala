package civilization.helper.battle

import civilization.gameboard.{GameBoard, PlayerDeck, WinnerLoot}
import civilization.helper._
import civilization.objects._


object BattleActions {

  private def numberOfCivUnitsToBattle(b: GameBoard, p: P): (Int, Option[Civilization.T]) = {
    val m: MapSquareP = getSquare(b, p)
    var battleforce: Int = UNITSBATTLE
    // barbarians village
    if (m.s.hvhere) return (battleforce, None)
    val civ : PlayerDeck = b.playerDeck(m.civHere.get)
    val li: PlayerLimits = getLimits(b, civ)
    // increase for city
    if (m.s.cityhere) battleforce = battleforce + 3
    // figures
    else battleforce = battleforce + (m.s.figures.numberofArmies -1) * 2
    // premium for fundamentalism
    if (GovernmentFeatures.increaseBattleHand(b.playerDeck(civ).gover)) battleforce = battleforce + 1
    // if scouts only are under attacks, 0 units.
    (if (m.s.figures.numberofArmies ==0) 0 else battleforce, m.civHere)
  }

  private def assemblyForce(b: GameBoard, p: P): Seq[CombatUnit] = {
    val n = numberOfCivUnitsToBattle(b, p)
    var u: Seq[CombatUnit] = Nil
    if (n._2.isEmpty) {
      // barbarians
      u = getThreeRandomUnits(b, false)
    }
    else {
      // civilization, draw units for battle
      val pl: PlayerDeck = b.playerDeck(n._2.get)
      val nounits = math.min(pl.units.length, n._1)
      u = getRandom(pl.units, nounits)._1
    }
    u
  }

  def assemblyCombatForces(b: GameBoard, civ: Civilization.T, p: P): BattleStart = {
    val mo: PlayerMove = getCurrentMove(b, civ).get
    val attacker: P = mo.lastp
    val attackerF: Seq[CombatUnit] = assemblyForce(b, attacker)
    val defenderF: Seq[CombatUnit] = assemblyForce(b, p)
    BattleStart(attackerF, defenderF)
  }

  def winnerLoot(b: GameBoard): Seq[WinnerLoot] = {
    val ba: (P, P) = battleParticipants(b)
    val att: MapSquareP = getSquare(b, ba._1)
    val defe: MapSquareP = getSquare(b, ba._2)
    if (defe.s.hvhere) return Nil
    var civloser: PlayerDeck = b.playerDeck(att.civHere.get)
    if (b.battle.get.attackerwinner) civloser = b.playerDeck(defe.civHere.get)
    // construct loot list
    var loot: Seq[WinnerLoot] = Nil
    val t: TradeForCiv = numberofTrade(b, civloser)
    if (t.trade > 0) loot = List(WinnerLoot(None, None, true, false))
    // hut
    val pl: PlayerDeck = civloser
    if (pl.hvlist.find(_.hv == HutVillage.Hut).isDefined)
      loot = loot :+ WinnerLoot(Some(HutVillage.Hut), None, false, false)
    // village
    if (pl.hvlist.find(_.hv == HutVillage.Village).isDefined)
      loot = loot :+ WinnerLoot(Some(HutVillage.Village), None, false, false)
    // resource
    loot = loot ++ pl.resou.table.filter(_._2 > 0).map(r => WinnerLoot(None, Some(r._1), false, false))
    loot
  }

}
