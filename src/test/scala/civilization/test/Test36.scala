package civilization.test

import civilization.I
import civilization.gameboard._
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
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
    val e = getCoins(gg, gg.playerDeck(Civilization.Rome))
    println(e.coins)
    assert(e.coins == 1)
    // numer of cards
    val pl: PlayerDeck = gg.playerDeck(Civilization.Rome)
    println(pl.cultureresource.cards.length)
    assert(pl.cultureresource.cards.length == 2)
    val li: PlayerLimits = getLimitsH(gg, Civilization.Rome)
    println(li.handsize)
    assert(li.handsize == 2)
    // build city
    Helper.executeCommandH(token, "SETCITY", 6, 6)
    gg = I.getBoardForToken(token)
    val pl1: PlayerDeck = gg.playerDeck(Civilization.Rome)
    println(pl1.cultureresource.cards.length)
    assert(pl1.cultureresource.cards.length == 3)
    var l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    // discard card immediately
    assert(l contains Command.DISCARDCARD)
    assert(!(l contains Command.ENDOFPHASE))
  }

  test("Research lost ") {
    val reg = Helper.ReadAndPlayForTwo("test36/BOARDGAME4.json", "test36/PLAY4.json", Civilization.Russia, Civilization.Spain)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    // research lost for Spain ??
    val trade = numberofTradeH(gg, Civilization.Spain)
    println(trade.trade)
    val le = techologyLevel(gg, gg.playerDeck(Civilization.Spain))
    println(le)
    assert(l contains Command.RESEARCH)
  }

  test("Two players game, one lost battle with village, loot ? ") {
    val reg = Helper.ReadAndPlayForTwo("test36/BOARDGAME5.json", "test36/PLAY5.json", Civilization.Russia, Civilization.Spain)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val ba = gg.battle.get
    println(ba)
    assert(ba.endofbattle)
    assert(!ba.attackerwinner)
    val wi = BattleActions.winnerLoot(gg)
    println(wi)
    // no loot
    assert(wi.loot == 0)
    assert(wi.list.isEmpty)
    val param =
      """ []  """
    // end battle, village, Russia, empty list of loot
    Helper.executeCommandH(token, "ENDBATTLE", -1, -1, param)
    gg = I.getBoardForToken(token)
    assert(gg.battle.isEmpty)
  }

  test("Research twice ?") {
    val reg = Helper.ReadAndPlayForTwo("test36/BOARDGAME6.json", "test36/PLAY6.json", Civilization.Russia, Civilization.Spain)
    val tokenR = reg._1
    val tokenS = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenR)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(!(l contains Command.RESEARCH))
    var l1 = allowedCommandsH(gg, Civilization.Russia)
    println(l1)
    assert(!(l1 contains Command.RESEARCH))
  }

  test("Logistic, unit level") {
    val reg = Helper.ReadAndPlayForTwo("test36/BOARDGAME6.json", "test36/PLAY6.json", Civilization.Russia, Civilization.Spain)
    val tokenR = reg._1
    val tokenS = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenS)
    val cl = gg.playerDeck(Civilization.Spain).combatlevel
    println(cl.getStrength(CombatUnitType.Aircraft))
    assert(cl.getStrength(CombatUnitType.Mounted) == 1)
    assert(cl.getStrength(CombatUnitType.Artillery) == 1)
    assert(cl.getStrength(CombatUnitType.Infantry) == 1)
  }

  test("Russia  lost research") {
    val reg = Helper.ReadAndPlayForTwo("test36/BOARDGAME6.json", "test36/PLAY7.json", Civilization.Russia, Civilization.Spain)
    val tokenR = reg._1
    val tokenS = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenS)
    var l = allowedCommandsH(gg, Civilization.Russia)
    println(l)
    assert(l contains Command.RESEARCH)

  }

}