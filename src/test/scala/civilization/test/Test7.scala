package civilization.test

import civilization.gameboard.GameBoard
import civilization.io.readdir._
import civilization.objects._
import civilization.io.tojson._
import civilization.io.fromjson._
import play.api.libs.json._
import org.scalatest.FunSuite
import civilization.I.II._
import civilization.helper._
import civilization.message.Mess
import civilization.io.readdir.GenBoard.genBoard


class Test7 extends FunSuite {

  Helper.I

  private def eqr(r : Resource.T,sq1: Square) = sq1.resource.isDefined && sq1.resource.get == r

  test("Write basic") {
    var j: JsValue = writeCivilizationT(Civilization.Rome)
    println(j)

    j = writeHutVillage(HutVillage(HutVillage.Village, Resource.Spy))
    println(j)
    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    assert(g != null)
    val ss = getSquare(g, P(0, 0))
    println(ss)
    assert(Terrain.Mountain == ss.terrain)
    assert(eqr(Resource.Coin,ss.sm))
    // verify
    var se: Set[String] = Set()
    g.map.map.foreach(mm => {
      assert(!(se contains (mm.tname)))
      se = se + mm.tname
    })
    j = writeResources(g.resources)
    println(j)
    println(Json.prettyPrint(j))

    j = writeSeqPlayerDeck(g.players)
    println(j)

    j = writeSeqOfMapTile(g.map.map)

    println(Json.prettyPrint(j))

    println("write gane board")
    j = writesGameBoard(g)
    println(Json.prettyPrint(j))
    println("try to read it again")

    val gg = toGameBoard(j)
    assert(g != null)
  }

  test("Register") {

    val s: String = getData(REGISTEROWNER, "Germany")
    println(s)
    val b: String = getData(GETBOARDGAME, s)
    println(b)
    val ll = getData(LISTOFCIV, null)
    println(ll)

  }

  test("Gen BoardGJ") {

    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")

    //    println(writesGameBoard(g))
    val ss = getSquare(g, P(0, 0))
    println(ss)
    // test
    val p: Seq[MapSquareP] = allSquares(g)

    val jj = genboardj.genBoardGameJson(g, Civilization.Germany)
    println(Json.prettyPrint(jj))
  }

  test("Execute command") {
    val token: String = getData(REGISTEROWNER, "Germany")
    var m: String = executeCommand(token, "SETCAPITAL", 2, 3, null)
    println(m)
    assert(m != null)
    m = executeCommand(token, "SETCAPITAL", 2, 2, null)
    println(m)
    assert(m == null)
    m = executeCommand(token, "SETCAPITAL", 2, 2, null)
    println(m)
    assert(m != null)
  }

}
