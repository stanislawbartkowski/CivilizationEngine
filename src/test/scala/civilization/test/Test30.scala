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


class Test30 extends AnyFunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.X

  test("Germany bonus") {
//    val token: String = II.getData(II.REGISTEROWNER, "Germany")
    val token : String = registerOwner("Germany")
    println(token)
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Germany)
    println(pl.units.length)
    assert(pl.units.length == 5)
    // three infantry
    assert(pl.units.filter(_.utype == CombatUnitType.Infantry).length == 3)
  }

  test("Technology upgrade unit") {
    val reg = Helper.readBoardAndPlayT("test30/BOARDGAME1.json", "test30/PLAY1.json", Civilization.Germany)
    val token = reg._1
    Helper.executeCommandH(token, "RESEARCH", -1, -1,""""Democracy"""")
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Germany)
    CombatUnitType.values.foreach(m => {
      println(pl.combatlevel.getStrength(m))
    })
    // infantry upgraded
    assert(pl.combatlevel.getStrength(CombatUnitType.Infantry) == 1)
    println(pl.units.length)
    // additional unit
    assert(pl.units.length == 6)
    println(pl.takefreeResources)
    assert(pl.takefreeResources > 0)
    var l = allowedCommandsH(gg, Civilization.Germany)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "GETFREERESOURCE")
    println(ite)
    Helper.executeCommandH(token, "GETFREERESOURCE", -1, -1,""""Incense"""")
    gg = I.getBoardForToken(token)
    val noi = gg.playerDeck(Civilization.Germany).resou.nof(Resource.Incense)
    assert(noi == 1)
    val pl1 = gg.playerDeck(Civilization.Germany)
    println(pl1.takefreeResources)
    assert(pl1.takefreeResources == 0)
  }

  test("Technology navigation, upgrade for 2 type of units") {
    val reg = Helper.readBoardAndPlayT("test30/BOARDGAME2.json", "test30/PLAY2.json", Civilization.Germany)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var pl = gg.playerDeck(Civilization.Germany)
    println(pl.units.length)
    assert(pl.units.length == 5)
    Helper.executeCommandH(token, "RESEARCH", -1, -1,""""Logistics"""")
    // should be eight
    gg = I.getBoardForToken(token)
    pl = gg.playerDeck(Civilization.Germany)
    println(pl.units.length)
    assert(pl.units.length == 8)
    // free resource
    var l = allowedCommandsH(gg, Civilization.Germany)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "GETFREERESOURCE")
    println(ite)
    assert(l contains Command.GETFREERESOURCE)
    // take 3 resources
    Helper.executeCommandH(token, "GETFREERESOURCE", -1, -1,""""Incense"""")
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Germany)
    assert(l contains Command.GETFREERESOURCE)
    Helper.executeCommandH(token, "GETFREERESOURCE", -1, -1,""""Silk"""")
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Germany)
    assert(l contains Command.GETFREERESOURCE)
    Helper.executeCommandH(token, "GETFREERESOURCE", -1, -1,""""Iron"""")
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Germany)
    println(l)
    assert(!(l contains Command.GETFREERESOURCE))
  }

  test("Technology navigation, upgrade for 2 type of units, only 2 resources can be taken") {
    val reg = Helper.readBoardAndPlayT("test30/BOARDGAME3.json", "test30/PLAY3.json", Civilization.Germany)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var pl = gg.playerDeck(Civilization.Germany)
    var l = allowedCommandsH(gg, Civilization.Germany)
    println(l)
    assert(!(l contains Command.GETFREERESOURCE))
  }

  test("Rome bonus") {
//    val token: String = II.getData(II.REGISTEROWNER, "Rome")
    val token : String = registerOwner("Rome")
    println(token)
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "SETCAPITAL")
    println(ite)
    Helper.executeCommandH(token, "SETCAPITAL", 2, 2)
    gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Rome)
    println(pl.cultureprogress)
    // bonus only for city, not capital
    // culture progress 1
    assert(pl.cultureprogress == 0)
    println(pl.cultureresource.cards.length)
    // one culture card
    assert(pl.cultureresource.cards.length == 0)
  }

  test("Rome bonus after building a wonder") {
    val reg = Helper.readBoardAndPlayT("test30/BOARDGAME4.json", "test30/PLAY4.json", Civilization.Rome)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var pl = gg.playerDeck(Civilization.Rome)
    println(pl.cultureprogress)
    // culture progress 2
    assert(pl.cultureprogress == 2)
    println(pl.cultureresource.cards.length)
    // two culture cards
    assert(pl.cultureresource.cards.length == 2)
  }

  test("Rome bonus after building a conquiering a village") {
    val reg = Helper.readBoardAndPlayT("test30/BOARDGAME4.json", "test30/PLAY5.json", Civilization.Rome)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var pl = gg.playerDeck(Civilization.Rome)
    println(pl.cultureprogress)
    // culture progress 3
    assert(pl.cultureprogress == 3)
    println(pl.cultureresource.cards.length)
    // two culture cards
    assert(pl.cultureresource.cards.length == 2)
    println(pl.cultureresource.persons.length)
    // single great person
    assert(pl.cultureresource.persons.length == 1)
  }

  test("Stonehedge") {
    val reg = Helper.readBoardAndPlayT("test30/BOARDGAME6.json", "test30/PLAY6.json", Civilization.China)
    val token = reg._1
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1,""""Research"""")
    var gg = I.getBoardForToken(token)
    var pl = gg.playerDeck(Civilization.China)
    println(pl.resou.nof(Resource.Culture))
    // one culture
    assert(pl.resou.nof(Resource.Culture) == 1)
  }

  }