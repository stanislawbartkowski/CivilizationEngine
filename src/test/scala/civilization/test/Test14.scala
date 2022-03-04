package civilization.test

import org.scalatest.funsuite.AnyFunSuite
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.objects._
import civilization.io.fromjson._
import civilization.io.readdir.GenBoard.genBoard
import civilization.io.tojson._
import civilization.io.readdir._
import play.api.libs.json.{JsArray, JsValue}
import Helper.{numof}

class Test14 extends AnyFunSuite with ImplicitMiximFromJson {

    Helper.X

  test("Test JSON combat units") {
      val s : String = """{ "name" : "Infantry", "strength": [1,2,3,4] }""";
      val j : JsValue = toJ(s);
      val u : CombatUnit = toCombatUnit(j)
      assert (CombatUnitType.Infantry == u.utype)
      assert( 4 == u.strength.length)
    }

     test("Write JSON combat unit") {
       val u : CombatUnit = CombatUnit(CombatUnitType.Aircraft, Array[Int](7,8,9,15))
       val j : JsValue = writeCombatUnit(u)
       println(j)
       val uu = toCombatUnit(j)
       assert(CombatUnitType.Aircraft == uu.utype)
       assert(15 == uu.strength(3))
     }

     test("Read list of units") {
       val l : Seq[CombatUnit] = readListOfUnits
       println(l)
       assert (l.length > 0)
     }

  test("Test gen board") {
    println("Test gen")
    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    assert(g != null)
    assert (!g.market.units.isEmpty)
    assert(g.market.killedunits.isEmpty)
    g.playerDeck(Civilization.Germany).units.foreach(println)
    assert(3 == g.playerDeck(Civilization.Germany).units.length)
    val numofI = numof(g,CombatUnitType.Infantry)
    val numofM = numof(g,CombatUnitType.Mounted)
    val numofA = numof(g,CombatUnitType.Artillery)
    println(numofI)
    val l : Seq[CombatUnit] = getThreeRandomUnits(g,true)
    // do not remove
    assert(numof(g,CombatUnitType.Infantry) == numofI -1 )
    assert(numof(g,CombatUnitType.Mounted) == numofM -1)
    assert(numof(g,CombatUnitType.Artillery) == numofA - 1)
  }



}
