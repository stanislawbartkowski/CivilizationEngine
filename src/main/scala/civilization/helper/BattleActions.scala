package civilization.helper

import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.objects._


object BattleActions {

  // ========================================
  // battle
  // ========================================

  private def numberOfCivUnitsToBattle(b: GameBoard, p: P): (Int, Option[Civilization.T]) = {
    val m: MapSquareP = getSquare(b, p)
    var battleforce: Int = UNITSBATTLE
    // barbarians village
    if (m.s.hvhere) return (battleforce, None)
    val li: PlayerLimits = getLimits(b, m.civHere.get)
    // increase for city
    if (m.s.cityhere) battleforce = battleforce + 3
    // figures
    else battleforce = battleforce + m.s.figures.numberofArmies * 2
    // premium for fundamentalism
    if (li.isFundametialism) battleforce = battleforce + 1
    (battleforce, m.civHere)
  }

  private def assemblyForce(b: GameBoard, p: P): Seq[CombatUnit] = {
    val n = numberOfCivUnitsToBattle(b, p)
    var u: Seq[CombatUnit] = Nil
    if (n._2.isEmpty) {
      // barbarians
      u = getThreeRandomUnits(b)
      // important: put them back again, will be removed in next step
      b.market.units = b.market.units ++ u
    }
    else {
      // civilization, draw units for battle
      val pl: PlayerDeck = b.playerDeck(n._2.get)
      while (u.length < n._1 && !pl.units.isEmpty) {
        val removed = getRandomRemove[CombatUnit](pl.units)
        u = u :+ removed._1
        pl.units = removed._2
      }
      // add them again, will be removed later
      pl.units = pl.units ++ u
    }
    u
  }

  implicit private def toBattleArmy(u: Seq[CombatUnit]): BattleArmy = BattleArmy(u.map(u => BattleUnit(Some(u), 0)) toArray)

  def assemblyCombatForces(b: GameBoard, civ: Civilization.T, p: P): BattleStart = {
    val mo: PlayerMove = getCurrentMove(b, civ).get
    val attacker: P = mo.lastp
    val attackerF: Seq[CombatUnit] = assemblyForce(b, attacker)
    val defenderF: Seq[CombatUnit] = assemblyForce(b, p)
    BattleStart(attackerF, defenderF)
  }

}
