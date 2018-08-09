package civilization.helper.battle

import civilization.gameboard.{GameBoard, PlayerDeck, WinnerLoot, WinnerLootEffect}
import civilization.helper._
import civilization.io.readdir.GameResources
import civilization.objects._


object BattleActions {

  private def numberOfCivUnitsToBattle(b: GameBoard, p: P): (Int, Option[Civilization.T]) = {
    val m: MapSquareP = getSquare(b, p)
    var battleforce: Int = UNITSBATTLE
    // barbarians village
    if (m.s.hvhere) return (battleforce, None)
    val civ: PlayerDeck = b.playerDeck(m.civHere.get)
    val li: PlayerLimits = getLimits(b, civ)
    // increase for city
    if (m.s.cityhere) battleforce = battleforce + 3
    // figures
    else battleforce = battleforce + (m.s.figures.numberofArmies - 1) * 2
    // premium for fundamentalism
    if (GovernmentFeatures.increaseBattleHand(b.playerDeck(civ).gover)) battleforce = battleforce + 1
    // if scouts only are under attacks, 0 units.
    (if (m.s.figures.numberofArmies == 0 && m.s.city.isEmpty) 0 else battleforce, m.civHere)
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


  private def coinonsheets(pl: PlayerDeck, loot: Int): Seq[WinnerLootEffect] =
    Seq.tabulate(pl.resou.nof(Resource.Coin))(n => WinnerLootEffect(LootEffectName.coin, loot, None, None, None, Some(true)))

  private def coinontechnologies(pl: PlayerDeck, loot: Int): Seq[WinnerLootEffect] =
    pl.tech.flatMap(t => Seq.tabulate(t.coins)(n => WinnerLootEffect(LootEffectName.coin, loot, Some(t.tech), None, None, None)))

  private def cardstoloot(pl: PlayerDeck, loot: Int): Seq[WinnerLootEffect] =
    pl.cultureresource.cards.map(c => WinnerLootEffect(LootEffectName.card, loot, None, None, Some(GameResources.getCultureCard(c).level), None))

  private def technologyloot(b: GameBoard, you: PlayerDeck, pl: PlayerDeck): Seq[WinnerLootEffect] =
    listOfTechnologiestoLearn(b, you, pl).map(t => WinnerLootEffect(LootEffectName.tech, 2, Some(t), None, None, None))

  def winnerLoot(b: GameBoard): WinnerLoot = {
    val ba: (P, P) = battleParticipants(b)
    val att: MapSquareP = getSquare(b, ba._1)
    val defe: MapSquareP = getSquare(b, ba._2)
    if (defe.s.hvhere) return WinnerLoot(0, Nil)
    var lootlevel: Int = 0
    // lootlevel 2 only if attacker conquer the city
    if (defe.s.cityhere && b.battle.get.attackerwinner) lootlevel = 2
    else lootlevel = 1

    var civloser: PlayerDeck = b.playerDeck(att.civHere.get)
    if (b.battle.get.attackerwinner) civloser = b.playerDeck(defe.civHere.get)
    // construct loot list
    var loot: Seq[WinnerLootEffect] = Nil
    val t: TradeForCiv = numberofTrade(b, civloser)
    // trade loot, effect 1
    for (i <- 1 to math.min(t.trade / WINNERTRADELOOT, lootlevel))
      loot = loot :+ WinnerLootEffect(LootEffectName.trade, 1, None, None, None, None)

    // hut and villages, effect 1
    val pl: PlayerDeck = civloser
    loot = loot ++ pl.hvlist.filter(_.hv == HutVillage.Hut).map(hv => WinnerLootEffect(LootEffectName.hut, 1, None, None, None, None)).take(lootlevel)
    loot = loot ++ pl.hvlist.filter(_.hv == HutVillage.Village).map(hv => WinnerLootEffect(LootEffectName.village, 1, None, None, None, None)).take(lootlevel)

    // resource, effect 1
    // flat out all resources
    loot = loot ++ pl.resou.table.filter(r => Resource.isMarketResource(r._1)).flatMap(r => Seq.tabulate(r._2)(n => WinnerLootEffect(LootEffectName.resource, 1, None, Some(r._1), None, None)))

    // coins on the sheet
    loot = loot ++ coinonsheets(pl, 1)

    // cards to drop
    loot = loot ++ cardstoloot(pl, 1)

    // coins on technologies effect 1
    loot = loot ++ coinontechnologies(pl, 1)

    // culture loot
    for (i <- 1 to math.min(pl.resou.nof(Resource.Culture) / WINNERCULTURELOOT, lootlevel))
      loot = loot :+ WinnerLootEffect(LootEffectName.culture, 1, None, None, None, None)

    if (lootlevel > 1) {
      // effect 2 loots
      // cards to steal
      loot = loot ++ cardstoloot(pl, 2)

      // coins on technologies effect 1
      loot = loot ++ coinontechnologies(pl, 2)

      // coins on the sheet
      loot = loot ++ coinonsheets(pl, 2)

      // technology effect 2 level
      val you: PlayerDeck = b.playerDeck(att.civHere.get)
      loot = loot ++ technologyloot(b, you, pl)
    }

    WinnerLoot(lootlevel, loot)
  }

}
