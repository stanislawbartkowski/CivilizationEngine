package civilization.io.readdir

import civilization.gameboard._
import civilization.helper.{revealTile,getThreeRandomUnits}
import civilization.io.fromjson.{toArrayHutVillages, toSeqPatterMap}
import civilization.message.{FatalError, M, Mess}
import civilization.objects.{Civilization, HutVillage, TilesRead, CombatUnit}
import play.api.libs.json.JsValue

import scala.collection.mutable.Buffer

object GenBoard {

  private def readHutVillages: Array[HutVillage] = {
    val j: JsValue = readJSON("map/market", "HUTVILLAGES.json")
    toArrayHutVillages(j)
  }

  def genBoard(l: List[Civilization.T], patt: String): GameBoard = {
    val j: JsValue = readJSON("map/pattern", patt)
    val lpatt: Seq[PatterMap] = toSeqPatterMap(j)
    val tiles: Seq[TilesRead] = readListOfTiles
    val ra = scala.util.Random
    // extract only normal tiles
    var ntile: Buffer[TilesRead] = tiles.filter(_.tile.civ == null).toBuffer
    // transform list to set
    var sciv: Set[Civilization.T] = l.toSet
    var map: Seq[MapTile] = Nil
    lpatt.foreach(p => {
      var tile: TilesRead = null
      if (p.o != null) {
        if (sciv.isEmpty) throw FatalError(Mess(M.MORECIVTTILESTHENCIVDECLARED))
        // take first civ
        val civ: Civilization.T = sciv.head
        tile = tiles.find(_.tile.civ == civ).getOrElse(null)
        if (tile == null) throw FatalError(Mess(M.CANNOTFINDHOMETILEFORCIV, civ))
        // remove element from set
        sciv = sciv - civ
      }
      else {
        // get random
        val i: Int = ra.nextInt(ntile.length)
        tile = ntile(i)
        // remove used
        //ntile = H.removeElement(ntile,i)
        ntile.remove(i)
      }
      val t: MapTile = MapTile(tile.name, p.p, null, genEmptySquares)
      t.tile = tile.tile
      map = map :+ t
    })
    if (!sciv.isEmpty) throw FatalError(Mess(M.TOOMANYCIVREQUESTED))
    val players: List[PlayerDeck] = l.map(PlayerDeck(_,Nil,Nil))
    val units : Seq[CombatUnit] = readListOfUnits
    val market : Market = Market(units.toArray,Nil)
    val g: GameBoard = GameBoard(players, BoardMap(map), Resources(readHutVillages, Nil),market)
    g.tech = readTechnologies
    // reveal tiles
    lpatt.foreach(p => if (p.o != null) revealTile(g, p.o, p.p))
    // attach random three units
    g.players.foreach(p => p.units = getThreeRandomUnits(g))
    g
  }

}
