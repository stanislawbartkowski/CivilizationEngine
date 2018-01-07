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
    val c: String = II.getData(II.LISTOFRES)
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
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME3.json", "test19/PLAY3.json", Civilization.Arabs, Civilization.America)
    val tokenc: String = reg._1
    val tokena: String = reg._2
    var g: GameBoard = I.getBoardForToken(tokena)
    assert(g.battle.isDefined)
    assert(g.battle.get.endofbattle)
    val s: Seq[WinnerLoot] = BattleActions.winnerLoot(g)
    println(s)
    assert(s.find(_.trade).isDefined)
    assert(s.find(l => l.res.isDefined && l.res.get == Resource.Silk).isDefined)
    assert(s.find(l => l.hv.isDefined && l.hv.get == HutVillage.Hut).isDefined)
    var ss = II.getData(II.GETBOARDGAME, tokena)
    var js: JsValue = toJ(ss)
    var batt: JsValue = (js \ "board" \ "battle").get
    println(Json.prettyPrint(batt))
    val wi: JsArray = (batt \ "winnerloot").as[JsArray]
    println(wi)
    assert(wi.value.length == 4)
    val numar = numberofTrade(g, Civilization.Arabs)
    val numam = numberofTrade(g, Civilization.America)
    println(numar)
    println(numam)
    Helper.executeCommandH(tokena, "ENDBATTLE", -1, -1, "\"trade\"")
    g = I.getBoardForToken(tokena)
    val xnumar = numberofTrade(g, Civilization.Arabs)
    val xnumam = numberofTrade(g, Civilization.America)
    println(xnumar)
    println(xnumam)
    // cannot exceed 27
//    assert(numar.trade == (xnumar.trade - 3))
//    assert(numam.trade == xnumam.trade + 3)
    assert(xnumar.loottrade == 3)
    assert(xnumam.loottrade == -3)
  }

  test("Two players game, battle, battle resolution, take Hut") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME3.json", "test19/PLAY3.json", Civilization.Arabs, Civilization.America)
    val tokenc: String = reg._1
    val tokena: String = reg._2
    val g: GameBoard = I.getBoardForToken(tokena)
    println(g.playerDeck(Civilization.Arabs).hvlist)
    println(g.playerDeck(Civilization.America).hvlist)
    Helper.executeCommandH(tokena, "ENDBATTLE", -1, -1, "\"Hut\"")
    val gg: GameBoard = I.getBoardForToken(tokena)
    println(gg.playerDeck(Civilization.Arabs).hvlist)
    println(gg.playerDeck(Civilization.America).hvlist)
    // Arabs take one hut from America
    assert(g.playerDeck(Civilization.Arabs).hvlist.length + 1 == gg.playerDeck(Civilization.Arabs).hvlist.length)
    assert(g.playerDeck(Civilization.America).hvlist.length - 1 == gg.playerDeck(Civilization.America).hvlist.length)
  }

  test("Two players game, battle, battle resolution, take Silk") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME3.json", "test19/PLAY3.json", Civilization.Arabs, Civilization.America)
    val tokenc: String = reg._1
    val tokena: String = reg._2
    val g: GameBoard = I.getBoardForToken(tokena)
    println(g.playerDeck(Civilization.Arabs).resou.table)
    println(g.playerDeck(Civilization.Arabs).units.length)
    g.playerDeck(Civilization.Arabs).units.foreach(println)
    val numofA = g.playerDeck(Civilization.Arabs).units.length
    val numofAM = g.playerDeck(Civilization.America).units.length
    Helper.executeCommandH(tokena, "ENDBATTLE", -1, -1, "\"Silk\"")
    val gg: GameBoard = I.getBoardForToken(tokena)
    println(gg.playerDeck(Civilization.Arabs).resou.table)
    assert(g.playerDeck(Civilization.Arabs).resou.nof(Resource.Silk) + 1 == gg.playerDeck(Civilization.Arabs).resou.nof(Resource.Silk))
    assert(g.playerDeck(Civilization.America).resou.nof(Resource.Silk) - 1 == gg.playerDeck(Civilization.America).resou.nof(Resource.Silk))
    println(gg.playerDeck(Civilization.Arabs).units.length)
    gg.playerDeck(Civilization.Arabs).units.foreach(println)
    assert(gg.playerDeck(Civilization.Arabs).units.length == 5)
    assert(gg.playerDeck(Civilization.America).units.length == 0)
  }

  test("Two players game, battle, battle resolution, armies dissapearing") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME4.json", "test19/PLAY4.json", Civilization.Germany, Civilization.Egypt)
    val tokenc: String = reg._1
    val tokena: String = reg._2
    val g: GameBoard = I.getBoardForToken(tokena)
    println(g.playerDeck(Civilization.Germany).units.length)
    assert(3 == g.playerDeck(Civilization.Germany).units.length)
    println(g.playerDeck(Civilization.Egypt).units.length)
    assert(g.playerDeck(Civilization.Egypt).units.isEmpty)
  }

  test("Two players game, battle, battle resolution, empty json for the second time") {
    val reg = Helper.ReadAndPlayForTwo("test19/BOARDGAME3.json", "test19/PLAY3.json", Civilization.Arabs, Civilization.America)
    val tokenc: String = reg._1
    val tokena: String = reg._2
    var j : String = II.getData(II.GETBOARDGAME,tokenc)
    println(j)
    assert (j != "")
    // again
    j = II.getData(II.GETBOARDGAME,tokenc)
    println("j="+j)
    assert(j == "")
    j = II.getData(II.GETBOARDGAME,tokena)
    println(j)
    assert (j != "")
    // again
    j = II.getData(II.GETBOARDGAME,tokena)
    println("j="+j)
    assert(j == "")
    // make move
    Helper.executeCommandH(tokena, "ENDBATTLE", -1, -1, "\"Silk\"")
    // again
    j = II.getData(II.GETBOARDGAME,tokena)
    println(j)
    // not empty for the first time
    assert (j != "")
    // again
    j = II.getData(II.GETBOARDGAME,tokena)
    println("j="+j)
    assert(j == "")
  }
  }