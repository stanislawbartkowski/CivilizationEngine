package civilization.test

import civilization.I
import civilization.I.executeCommand
import civilization.gameboard._
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._
import civilization.helper.battle.BattleActions
import civilization.io.readdir.GameResources

class Test36 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Great person, Henry Fond") {
    val reg = Helper.readBoardAndPlayT("test36/BOARDGAME1.json", "test36/PLAY1.json", Civilization.Rome)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val pl: PlayerLimits = getLimitsH(gg, Civilization.Rome)
    println(pl.travelSpeed)
    // HenryFord is increasing speed by one
    assert(pl.travelSpeed == 3)
  }

  test("Problem with Rome feature, advance culture") {
    val reg = Helper.readBoardAndPlayT("test36/BOARDGAME2.json", "test36/PLAY2.json", Civilization.Rome)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    // {"command":"EXPLOREHUT","civ":"Rome","p":{"row":4,"col":6},"param":null}
    var l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    assert(l contains Command.EXPLOREHUT)
    val pl: PlayerDeck = gg.playerDeck(Civilization.Rome)
    println(pl.cultureprogress)
    Helper.executeCommandH(token, "EXPLOREHUT", 4, 6)
    gg = I.getBoardForToken(token)
    val pl1: PlayerDeck = gg.playerDeck(Civilization.Rome)
    println(pl1.cultureprogress)
    // is OK
    assert(pl1.cultureprogress == 1)
  }

//  {"command":"SETCITY","civ":"Rome","p":{"row":6,"col":6},"param":null},
//  {"command":"ADVANCECULTUREFORFREE","civ":"Rome","p":null,"param":null},
//  {"command":"CULTURECARD","civ":"Rome","p":null,"param":"BreadandCircuses"},
//  {"command":"GREATPERSONPUT","civ":"Rome","p":{"row":5,"col":2},"param":{"p":{"row":6,"col":1},"greatperson":"AdamSmith"}},
//  {"command":"ENDOFPHASE","civ":"Rome","p":null,"param":"StartOfTurn"}


  test("Something wrong with Rome advance culture") {
    val reg = Helper.readBoardAndPlayT("test36/BOARDGAME3.json", "test36/PLAY3.json", Civilization.Rome)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val e = getCoins(gg,gg.playerDeck(Civilization.Rome))
    println(e.coins)
    assert(e.coins == 1)
    // numer of cards
    val pl : PlayerDeck = gg.playerDeck(Civilization.Rome)
    println(pl.cultureresource.cards.length)
    assert(pl.cultureresource.cards.length == 2)
    val li : PlayerLimits = getLimitsH(gg,Civilization.Rome)
    println(li.handsize)
    assert(li.handsize == 2)
    // build city
    Helper.executeCommandH(token, "SETCITY", 6, 6)
    gg = I.getBoardForToken(token)
    val pl1 : PlayerDeck = gg.playerDeck(Civilization.Rome)
    println(pl1.cultureresource.cards.length)
    assert(pl1.cultureresource.cards.length == 3)
    var l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    // discard card immediately
    assert(l contains Command.DISCARDCARD)
    assert(!(l contains Command.ENDOFPHASE))


  }
}