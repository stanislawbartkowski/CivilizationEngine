package civilization.io.readdir

import civilization.gameboard._
import civilization.helper.{revealTile, getThreeRandomUnits, getRandomHutVillage, getRandom}
import civilization.io.fromjson.{toArrayHutVillages, toSeqPatterMap}
import civilization.message.{FatalError, M, Mess}
import civilization.objects._
import play.api.libs.json.JsValue
import civilization.io.fromjson.ImplicitMiximFromJson

import scala.collection.mutable.Buffer

object GenBoard extends ImplicitMiximFromJson {

  private def readHutVillages: Array[HutVillage] = {
    val j: JsValue = readJSON("map/market", "HUTVILLAGES.json")
    toArrayHutVillages(j)
  }

  private def readResources(nciv: Int): BoardResources = {
    val j: JsValue = readJSON("map/market", "RESOURCES.json")
    val g: BoardResources = j.as[BoardResources]
    // resources available should be equal to number of civilizations in play
    Resource.values.foreach(r => if (g.nof(r) == -1) g.setResNum(r, nciv))
    g
  }

  private def assignResources(b: GameBoard, m: MapTile) = {
    for (row <- 0 until m.mapsquares.length; col <- 0 until m.mapsquares(row).length)
      if (m.tile.terrain(row)(col).hv != null) m.mapsquares(row)(col).hv = Some(getRandomHutVillage(b, m.tile.terrain(row)(col).hv))
  }

  def genBoard(l: List[Civilization.T], patt: String): GameBoard = {
    val j: JsValue = readJSON("map/pattern", patt)
    val lpatt: Seq[PatternMap] = toSeqPatterMap(j)
    var rtiles: Seq[TilesRead] = readListOfTiles.filter(!_.tile.civhome)
    val tilesciv: Seq[TilesRead] = readListOfTiles.filter(_.tile.civhome)
    val civs: Seq[CivilizationG] = readListOfCivs
//    val techs: Seq[Technology] = readTechnologies
    // transform list to set
    var sciv: Set[Civilization.T] = l.toSet
    var map: Seq[MapTile] = Nil
    lpatt.foreach(p => {
      var tile: TilesRead = null
      if (p.o.isDefined) {
        if (sciv.isEmpty) throw FatalError(Mess(M.MORECIVTTILESTHENCIVDECLARED))
        // take first civ
        val civ: Civilization.T = sciv.head
        tile = tilesciv.find(_.tile.civ == civ).getOrElse(null)
        if (tile == null) throw FatalError(Mess(M.CANNOTFINDHOMETILEFORCIV, civ))
        // remove element from set
        sciv = sciv - civ
      }
      else {
        var r = getRandom(rtiles, 1)
        tile = r._1.head
        rtiles = r._2
      }
      val t: MapTile = MapTile(tile.name, p.p, None, genEmptySquares)
      t.tile = tile.tile
      map = map :+ t
    })
    if (!sciv.isEmpty) throw FatalError(Mess(M.TOOMANYCIVREQUESTED))
    val players: List[PlayerDeck] = l.map(c => {
      val civ: CivilizationG = civs.find(_.civ == c).get
      val pt: PlayerTechnology = PlayerTechnology(civ.tech, Some(true))
      PlayerDeck(c, List(pt), Nil, new BoardResources(), civ.gover)
    }
    )

    val units: Seq[CombatUnit] = readListOfUnits
    val market: Market = Market(units, Nil)
    val g: GameBoard = GameBoard(players, BoardMap(map), Resources(readHutVillages, Nil, readResources(l.length)), market)
    // reveal tiles
    lpatt.foreach(p => if (p.o.isDefined) revealTile(g, p.o.get, p.p))
    // attach random three units
    g.players.foreach(p => p.units = getThreeRandomUnits(g, true))
    // assing resources to hut and villages
    map.foreach(m => assignResources(g, m))
    g
  }

}
