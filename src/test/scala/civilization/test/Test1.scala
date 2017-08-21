package civilization.test

import civilization.io.readdir._
import org.scalatest.FunSuite
import play.api.libs.json.{JsPath, JsValue}
import civilization.io.fromjson._
import civilization.objects._
import civilization.gameboard._

import scala.reflect.io.File
import scala.io.Source


class Test1 extends FunSuite {

  test("Test that we can read resource/map/tile without exception") {
    val l: List[(String, JsValue)] = readdirJSON("map/tiles")
    assert(!l.isEmpty)
    l.foreach(println)
    l.foreach(p => assert(p._2.isInstanceOf[JsValue]))
  }

  test("Read tokens json") {
    val l: JsValue = readTestJSON("resources/map/tiles/TOKEN1.json")
    println(l)
    val t: Tokens = convert[TokensJ](TokensJ(l))
    println(t)
    assert(t.numofCulture == 1)
    assert(t.numofProduction == 2)
    assert(t.numofTrade == 3)
  }

  test("Read square json") {
    val l: JsValue = readTestJSON("resources/map/tiles/SQUARE1.json")
    println(l)
    val t: Square = convert[SquareJ](SquareJ(l))
    println(t)
    assert(Resource.Incense == t.resource)
    assert(Terrain.Desert == t.terrain)
    assert(!t.naturalwonder)
    assert(t.hv == null)
    assert(t.token.numofTrade == 1)
    assert(t.token.numofProduction == 0)
  }

  test("Read square json without resource") {
    val l: JsValue = readTestJSON("resources/map/tiles/SQUARE2.json")
    println(l)
    val t: Square = convert[SquareJ](SquareJ(l))
    println(t)
    assert(t.resource == null)
    assert(Terrain.Mountain == t.terrain)
  }

  test("Read square json with natural wonder and hut") {
    val l: JsValue = readTestJSON("resources/map/tiles/SQUARE3.json")
    println(l)
    val t: Square = convert[SquareJ](SquareJ(l))
    println(t)
    assert(t.resource == null)
    assert(Terrain.Mountain == t.terrain)
    assert(t.naturalwonder)
    assert(HutVillage.Hut == t.hv)
  }

  test("Read seq square") {
    val l: JsValue = readTestJSON("resources/map/tiles/SEQ1.json")
    println(l)
    val s: Array[Square] = (JsPath).read[Array[Square]].reads(l).get
    println(s)
    assert(4 == s.length)
    s.foreach(println)
    val ss: Square = s(0)
    assert(Terrain.Mountain == ss.terrain)
    assert(1 == ss.token.numofProduction)
  }

  test("Read seq[seq] square") {
    val l: JsValue = readTestJSON("resources/map/tiles/SEQ2.json")
    println(l)
    val s: Array[Array[Square]] = (JsPath).read[Array[Array[Square]]].reads(l).get
    assert(4 == s.length)
    for (i <- 0 until 4) {
      println("row = " + i)
      for (j <- 0 until 4) {
        println(i + " : " + j)
        val sq: Square = s(i)(j)
        println(sq)
        assert(null != sq.token)
      }
    }
    val sq1: Square = s(0)(1)
    assert(Resource.Incense == sq1.resource)
    val sq2: Square = s(0)(3)
    assert(Resource.Silk == sq2.resource)
  }

  test("Read tile from JSON ") {
    val l: JsValue = readTestJSON("resources/map/tiles/TILE1.json")
    println(l)
    val t: Tile = convert[TileJ](TileJ(l))
    assert(Civilization.Rome == t.civ)
    assert(1 == t.suggestedcapital.row)
    assert(2 == t.suggestedcapital.col)
  }

  test("Read tile not civilized from JSON ") {
    val l: JsValue = readTestJSON("resources/map/tiles/TILE2.json")
    println(l)
    val t: Tile = convert[TileJ](TileJ(l))
    assert(null == t.civ)
    assert(null == t.suggestedcapital)
  }

  test("Read list of available tiles ") {
    val l: Seq[TilesRead] = readListOfTiles
    assert(!l.isEmpty)
    l.foreach(l => println(l.name + " " + l.tile))
  }

//  test("Read player from test ") {
//    val l: JsValue = readTestJSON("resources/map/tiles/PLAYER.json")
//    println(l)
//    val t: Player = convert[PlayerJ](PlayerJ(l))
//    println(t)
//    assert(Player.A == t.player)
//    assert(Civilization.Germany == t.civ)
//  }

  test("Read single map tile ") {
    val l: JsValue = readTestJSON("resources/map/tiles/MAP1.json")
    println(l)
    val t: MapTile = convert[MapTileJ](MapTileJ(l))
    println(t)
    assert("TILE1.json" == t.tname)
    assert(5 == t.p.row)
    assert(8 == t.p.col)
    assert(Orientation.Left == t.orientation)
    val ma: MapSquare = t.mapsquares(0)(0)
    println(ma)
    assert(ma.hvtaken)
    assert(ma.figures.civ == Civilization.Germany)
    assert(1 == ma.figures.numberofArmies)
    assert(2 == ma.figures.numberofScouts)
    assert(ma.city != null)
    assert(City.Normal == ma.city.citytype)
    assert(Civilization.Rome == ma.city.civ)
    println("-------------------")
    for (i <- 0 until 4)
      for (j <- 0 until 4) println(t.mapsquares(i)(j))
    val ma1: MapSquare = t.mapsquares(0)(1)
    println(ma1)
  }

  test("Read single playerdeck ") {
    val l: JsValue = readTestJSON("resources/map/tiles/PLAYERDECK1.json")
    println(l)
    val t: PlayerDeck = convert[PlayerDeckJ](PlayerDeckJ(l))
    println(t)
    assert(Civilization.Germany == t.civ)
  }

  test("Read single boardgame ") {
    val l: JsValue = readTestJSON("resources/map/tiles/BOARDGAME1.json")
    println(l)
    val t: GameBoard = convert[GameBoardJ](GameBoardJ(l))
    println(t)
    assert(1 == t.players.length)
    assert(2 == t.map.map.length)
  }

  test("Read single boardgame and read tiles") {
    val l: JsValue = readTestJSON("resources/map/tiles/BOARDGAME1.json")
    println(l)
    val g: GameBoard = readGameBoard(l)
    assert(2 == g.map.map.length)
    println("----------------------------")
    g.map.map.foreach(println)
    g.map.map.foreach(p => assert(p.tile != null))
  }

  test("Read single hutvillage") {
    val l: JsValue = readTestJSON("resources/map/tiles/HUTVILLAGE.json")
    println(l)
    val t: HutVillage = toHutVillage(l)
    println(t)
    assert(HutVillage.Hut == t.hv)
    assert(Resource.Spy == t.resource)
  }

  test("Read command line") {
    val l: JsValue = readTestJSON("resources/map/tiles/COMMAND1.json")
    println(l)
    val p = toParams(l)
    println(p)
    assert(p.command == Command.SETCAPITAL)
    assert(p.civ == Civilization.Germany)
  }

  test("Read techonlogies") {
    val l : Seq[Technology] = readTechnologies
    println(l)
    assert(l != null)
    assert(!l.isEmpty)
  }

  test("read null enum") {
    var c :  Orientation.T = toOrientation(toJ("\"Left\""))
    println(c)
    c = toOrientation(toJ("null"))
    println(c)
    assert(c == null)
  }

  test("Read tile ROME ") {
    val l: JsValue = readJSON("map/tiles","TILEROME.json")
    println(l)
    val t: Tile = convert[TileJ](TileJ(l))
    println(t)
    assert(Civilization.Rome == t.civ)
    assert(1 == t.suggestedcapital.row)
    assert(2 == t.suggestedcapital.col)
    val s : Square = t.terrain(1)(2)
    println(s)
    assert(s.terrain == Terrain.Grassland)
  }


}