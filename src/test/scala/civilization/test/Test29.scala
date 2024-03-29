package civilization.test

import civilization.I
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II}
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.{JsArray, JsValue}
import Helper._


class Test29 extends AnyFunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.X

  test("Egypt bonus") {
//    val token: String = II.getData(II.REGISTEROWNER, "Egypt")
    val token : String = registerOwner("Egypt")
    println(token)
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Egypt)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    val ss = II.itemizeCommand(token, "SETCAPITAL")
    println(ss)
    Helper.executeCommandH(token, "SETCAPITAL", 2, 2)
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    // FREE WONDER not visible
    assert(l contains Command.FREEWONDER)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "FREEWONDER")
    println(ite)
    val wo = gg.playerDeck(Civilization.Egypt).freeWonder.get
    println(wo)
    val c =
      """{"p":{"row":1,"col":1},"wonder":"""" + wo.toString() +""""}"""
    Helper.executeCommandH(token, "FREEWONDER", 2, 2, c)
    gg = I.getBoardForToken(token)
    val p: MapSquareP = getSquare(gg, P(1, 1))
    println(p)
    assert(p.s.wonder.get.w == wo)
    val wolist = gg.getCurrentWonders()
    wolist.foreach(println)
    assert(!(wolist contains wo))
    l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    assert(!(l contains Command.FREEWONDER))
  }

  test("Statue of Zeus ") {
    val reg = Helper.readBoardAndPlayT("test29/BOARDGAME1.json", "test29/PLAY1.json", Civilization.Egypt)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val co: PlayerLimits = getLimitsH(gg, Civilization.Egypt)
    println(co.combatBonus)
    assert(co.combatBonus == 6)
  }

  test("Free unlocked building") {
    val reg = Helper.readBoardAndPlayT("test29/BOARDGAME1.json", "test29/PLAY2.json", Civilization.Egypt)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    assert(l contains Command.FREEBUILDINGCITYACTION)
    assert(!(l contains Command.BUYBUILDING))
    val ss = II.itemizeCommand(token, "FREEBUILDINGCITYACTION")
    println(ss)
    val c =
      """{"p":{"row":1,"col":1},"building":"Market"}"""
    Helper.executeCommandH(token, "FREEBUILDINGCITYACTION", 2, 2, c)
    gg = I.getBoardForToken(token)
    // check 0,4, temple should be built
    val ma: MapSquareP = getSquare(gg, P(1, 1))
    println(ma)
    assert(ma.s.building.isDefined)
    assert(ma.s.building.get.name == BuildingName.Market)
    l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
  }

  test("Free unlocked building only once") {
    val reg = Helper.readBoardAndPlayT("test29/BOARDGAME1.json", "test29/PLAY3.json", Civilization.Egypt)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    assert(!(l contains Command.BUYBUILDING))
  }

}
