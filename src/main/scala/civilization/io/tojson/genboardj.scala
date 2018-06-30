package civilization.io.tojson

import civilization.gameboard._
import civilization.helper._
import civilization.objects._
import play.api.libs.json._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.io.tojson.BattleJSon.genBattleJson
import civilization.io.tojson.CombatUnitJSon.unitstoJSON

/** Generates game board in JSON to be picked up by UI. */

object genboardj {

  // production:
  // if empty square : number of original production
  // if city: number of city production
  case class MapSquareJ(revealed: Boolean, t: Terrain.T, trade: Int, production: Int, resource: Option[Resource.T], capForCiv: Option[Civilization.T],
                        civ: Civilization.T, city: City.T, defence: Int, numberofArmies: Int, numberofScouts: Int, tile: String, hv: Option[HutVillage.T], building: Option[BuildingName.T], wonder: Option[Wonders.T], culture: Int,
                        greatperson : Option[GreatPersonName.T], greatpersontype : Option[GreatPersonTypeName.T])

  case class PlayerTech(val pl: PlayerTechnology, val level: Int, val coins: Int)

  private def plToJson(pl: PlayerTech): JsValue = Json.obj(
    S.tech -> pl.pl.tech,
    S.initial -> pl.pl.initial,
    S.level -> pl.level,
    S.coins -> pl.coins
  )

  implicit def plSeqToJ(pl: Seq[PlayerTech]): Seq[JsValue] = pl.map(plToJson)

  case class PlayerDeckJ(civ: PlayerDeck, numberofTrade: Int, commands: Seq[Command.T], limits: PlayerLimits, technologylevel: Int, tech: Seq[PlayerTech], pl: PlayerDeck, wonders: Seq[Wonders.T], coins: Int)

  case class Game(active: Civilization.T, roundno: Int, phase: TurnPhase.T)

  case class BoardGameJ(g: Game, map: Array[Array[MapSquareJ]], you: PlayerDeckJ, others: Seq[PlayerDeckJ])


  private def producehv(hvcount: Map[HutVillage.T, Seq[HutVillage]], h: HutVillage.T): JsObject =
    if (hvcount.contains(h)) JsObject(Seq(h.toString -> JsNumber(hvcount(h).length))) else JsObject.empty

  private def hvtojson(hv: Seq[HutVillage], you: Boolean): JsValue = {
    // calculate the number of Hut and Villages
    val hvcount: Map[HutVillage.T, Seq[HutVillage]] = hv.groupBy(_.hv)
    val jhut: JsObject = producehv(hvcount, HutVillage.Village)
    val jvillage: JsObject = producehv(hvcount, HutVillage.Hut)
    val l: JsObject = if (you) Json.obj(S.list -> hv) else JsObject.empty
    return jhut ++ jvillage ++ l
  }

  private def contructSquareJ(b: GameBoard, ss: MapSquareP): MapSquareJ = {
    val t: Terrain.T = if (ss.revealed) ss.terrain else null;
    val trade: Int = if (ss.revealed) ss.numberOfTrade else -1;
    var civ: Civilization.T = if (ss.s.cityhere) ss.s.city.get.civ else null
    val production: Int = if (ss.revealed) (if (!ss.s.cityhere) ss.numberOfProduction else getProductionForCity(b, civ, ss.p).prod) else -1;
    val resource: Option[Resource.T] = if (ss.revealed) ss.resource else None
    val cap: Option[Civilization.T] = ss.suggestedCapitalForCiv
    var defence: Int = 0
    var city: City.T = null
    if (ss.s.cityhere) {
      city = ss.s.city.get.citytype
      defence = ss.s.city.get.defenceStrength()
    }
    var numberofArmies: Int = -1
    var numberofScouts: Int = -1
    if (!ss.s.figures.empty) {
      civ = ss.s.figures.civ
      numberofArmies = ss.s.figures.numberofArmies
      numberofScouts = ss.s.figures.numberofScouts
    }

    MapSquareJ(ss.revealed, t, trade, production, resource, cap, civ, city, defence, numberofArmies, numberofScouts, ss.t.tname,
      if (ss.s.hv.isDefined) Some(ss.s.hv.get.hv) else None,
      if (ss.s.building.isDefined) Some(ss.s.building.get.name) else None,
      if (ss.s.wonder.isDefined) Some(ss.s.wonder.get.w) else None,
      if (ss.s.cityhere) cultureForCity(b, ss.p).culture else 0,
      None,
      if (ss.s.greatperson.isDefined) Some(ss.s.greatperson.get.ptype) else None
    )
  }

  private def genGame(g: GameBoard, civrequesting: Civilization.T): Game = {

    val cu: CurrentPhase = currentPhase(g)
    // TODO: active civilization, later
    var civ: Civilization.T = cu.notcompleted.head
    if (!TurnPhase.turnAction(cu.turnPhase))
      if (cu.notcompleted.contains(civrequesting)) civ = civrequesting
    Game(civ, cu.roundno, cu.turnPhase)
  }

  private def wondersForPlayers(g: GameBoard, civ: Civilization.T): Seq[Wonders.T] =
    citiesForCivilization(g, civ).map(c => pointsAround(g, c.p)).flatten.map(p => getSquare(g, p)).filter(m => m.s.wonder.isDefined).map(m => m.s.wonder.get.w)


  private def genPlayerDeckJ(g: GameBoard, civ: PlayerDeck): PlayerDeckJ =
    PlayerDeckJ(civ, numberofTrade(g, civ).trade, allowedCommands(g, civ), getLimits(g, civ),
      techologyLevel(g, civ),
      civ.tech.map(t => PlayerTech(t, g.techlevel(t), t.coins)),
      civ, wondersForPlayers(g, civ),
      getCoins(g, civ).coins)

  private def commandToArray(l: Seq[Command.T]): JsArray = {
    JsArray(l.map(c => Json.obj(S.command -> c)).foldLeft(List[JsObject]())(_ :+ _))
  }


  private def genPlayerDeckJson(p: PlayerDeckJ, you: Boolean): JsValue = Json.obj(
    S.tech -> plSeqToJ(p.tech),
    S.gover -> p.pl.gover,
    S.civ -> p.civ,
    S.trade -> p.numberofTrade,
    "tradelevel" -> p.technologylevel,
    "commands" -> commandToArray(p.commands),
    "citylimit" -> p.limits.citieslimit,
    "armieslimit" -> p.limits.armieslimit,
    "scoutslimit" -> p.limits.scoutslimit,
    "tradeforprod" -> p.limits.tradeforProd,
    "prodfortrade" -> p.limits.prodfortrade,
    S.units -> unitstoJSON(p.pl.units, you, p.pl.combatlevel),
    S.resources -> p.pl.resou,
    S.hutvillages -> hvtojson(p.pl.hvlist, you),
    S.wonders -> p.wonders,
    S.coins -> p.coins,
    "combatbonus" -> p.limits.combatBonus,
    S.cultureprogress -> p.pl.cultureprogress,
    "handsize" -> p.limits.handsize,
    "travelspeed" -> p.limits.travelSpeed,
    "stacklimit" -> p.limits.stackinglimit,
    "cultureresource" -> CultureResourcesToJSon.cultureToJSon(p.pl.cultureresource, you)
  )

  private def genBoardGameJ(g: GameBoard, civ: PlayerDeck): BoardGameJ = {
    val p: Seq[MapSquareP] = allSquares(g)
    val maxrow: Int = p.map(_.p.row).max
    val maxcol: Int = p.map(_.p.col).max
    val map: Array[Array[MapSquareJ]] = Array.ofDim(maxrow + 1, maxcol + 1)
    // TODO: initialization to null maybe unnecessary, doublecheck
    for (i <- 0 to maxrow; j <- 0 to maxcol) map(i)(j) = null
    p.foreach(ss => map(ss.p.row)(ss.p.col) = contructSquareJ(g, ss))
    val others: Seq[PlayerDeckJ] = g.players.filter(_.civ != civ).map(c => genPlayerDeckJ(g, c))
    BoardGameJ(genGame(g, civ), map, genPlayerDeckJ(g, civ), others)
  }

  private def mapSquareJ(m: MapSquareJ): JsValue = {
    Json.obj("revealed" -> m.revealed, S.terrain -> Option(m.t), S.trade -> m.trade, "production" -> m.production, S.resource -> m.resource,
      "capciv" -> Option(m.capForCiv), S.civ -> Option(m.civ), S.city -> Option(m.city), "defence" -> m.defence, S.numberofArmies -> m.numberofArmies, S.numberofScouts -> m.numberofScouts,
      "tile" -> m.tile, S.hutvillage -> Option(m.hv),
      S.building -> m.building,
      S.wonder -> m.wonder,
      S.culture -> m.culture,
      S.greatperson -> m.greatperson,
      "greatpersontype" -> m.greatpersontype
    )
  }

  private def gameToJ(g: Game): JsValue = {
    Json.obj("roundno" -> g.roundno, S.phase -> g.phase, "active" -> g.active)
  }

  private def toSeqTech(li: Seq[Technology]): JsValue = {
    val l: Seq[JsValue] = li.map(writeTechonology)
    Json.toJson(l)
  }

  def genBoardGameJson(g: GameBoard, civ: PlayerDeck): JsValue = {
    val b: BoardGameJ = genBoardGameJ(g, civ)
    var rows: Seq[JsValue] = Seq()

    for (row <- 0 until b.map.length) {
      var cols: Seq[JsValue] = Nil
      for (col <- 0 until b.map(row).length) {
        cols = cols :+ mapSquareJ(b.map(row)(col))
      }
      rows = rows :+ JsArray(cols)
    }
    val o: Seq[JsValue] = b.others.map(genPlayerDeckJson(_, false))

    JsObject(Seq("board" -> JsObject(Seq(
      S.map -> JsArray(rows),
      "game" -> gameToJ(b.g),
      S.units -> unitstoJSON(g.market.units, false, null),
      S.killedunits -> unitstoJSON(g.market.killedunits, true, null),
      S.resources -> Json.toJson(g.resources.resou),
      S.hutvillagesused -> hvtojson(g.resources.hvused, true),
      S.you -> genPlayerDeckJson(b.you, true),
      "others" -> JsArray(o),
      // only first 4 wonders
      S.wonders -> writeListOfWondersNames(g.market.wonders.take(WONDERWINDOW)),
      S.battle -> genBattleJson(g, civ),
      S.buildings -> Json.toJson(g.market.buildings),
      "cultureused" -> CultureResourcesToJSon.cultureToJSon(g.cultureused, true)
    ))))
  }


}
