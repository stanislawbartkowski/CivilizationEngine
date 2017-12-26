package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.io.fromjson.{toJ, _}
import civilization.io.readdir.GenBoard.genBoard
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects._
import org.scalatest.FunSuite
import play.api.libs.json._
import civilization.helper._


class Test19 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("List of civs") {
    val c: String = II.getData(II.LISTOFCIV)
    System.out.println(c)
    val a: JsArray = Json.arr(c)
    assert(a != null)
  }

  test("Test gen board") {
    println("Test gen")
    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    assert(g != null)
    System.out.println(g.market.units)
    val num = g.market.units.filter(_.utype == CombatUnitType.Aircraft).length
    System.out.println(num)
    assert(8 == num)
  }

  test("Two players game, battle") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME1.json", "test19/PLAY1.json", Civilization.Russia, Civilization.Arabs)
    val token: String = reg._1
    val tokena: String = reg._2
    val gg: GameBoard = I.getBoardForToken(token)
    val l = allowedCommands(gg, Civilization.Arabs)
    println(l)
    assert(!l.filter(_ == Command.ATTACK).isEmpty)
    val ite: String = II.itemizeCommand(tokena, "ATTACK")
    println(ite)
    val j: JsValue = toJ(ite)
    val a: JsArray = (j \ "attack").as[JsArray]
    println(a)
    val p: P = a.value.head.as[P]
    assert(P(7, 4) == p)
  }

  test("Two players game, battle, attack") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME1.json", "test19/PLAY1.json", Civilization.Russia, Civilization.Arabs)
    val token: String = reg._1
    val tokena: String = reg._2
    Helper.executeCommandH(tokena, "ATTACK", 7, 4, null)
    var g: GameBoard = I.getBoardForToken(tokena)
    assert(g.battle.isDefined)
  }

  test("Two players game, battle, something wrong with city building") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME2.json", "test19/PLAY2.json", Civilization.China, Civilization.America)
    val tokenc: String = reg._1
    val tokena: String = reg._2
    var g: GameBoard = I.getBoardForToken(tokena)
    //    val b : PlayerDeck : g.playerDeck(Civilization.America)
    var nu = getNumberOfArmies(g, Civilization.China)
    println(nu)
    assert(nu._1 == 2)
    Helper.executeCommandH(tokenc, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    g = I.getBoardForToken(tokenc)
    //    val b : PlayerDeck : g.playerDeck(Civilization.America)
    nu = getNumberOfArmies(g, Civilization.China)
    println(nu)
    assert(nu._1 == 2)
  }

  test("Two players game, battle, battle resolution") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME3.json", "test19/PLAY3.json", Civilization.China, Civilization.America)
    val tokenc: String = reg._1
    val tokena: String = reg._2
    var g: GameBoard = I.getBoardForToken(tokena)
    assert(g.battle.isDefined)
    assert(g.battle.get.endofbattle)
    val s : Seq[WinnerLoot] = BattleActions.winnerLoot(g)
    println(s)
    assert(s.find(_.trade).isDefined)
    assert(s.find(l => l.res.isDefined && l.res.get == Resource.Silk).isDefined)
    assert(s.find(l => l.hv.isDefined && l.hv.get == HutVillage.Hut).isDefined)
    var ss = II.getData(II.GETBOARDGAME, tokena)
    var js: JsValue = toJ(ss)
    var batt: JsValue = (js \ "board" \ "battle").get
    println(Json.prettyPrint(batt))
    val wi : JsArray = (batt \ "winnerloot").as[JsArray]
    println(wi)
    assert(wi.value.length == 4)

  }
}