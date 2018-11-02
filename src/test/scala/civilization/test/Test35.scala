package civilization.test

import civilization.I
import civilization.gameboard.{GameBoard, WinnerLoot}
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._

class Test35 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("No research") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME1.json", "test35/PLAY1.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    assert(!(l contains Command.RESEARCH))
  }

  test("Democracy action") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME1.json", "test35/PLAY2.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    val tr: TradeForCiv = numberofTradeH(gg, Civilization.America)
    println(tr.trade)
    val e = getCoins(gg, gg.playerDeck(Civilization.America))
    println(e.coins)
    Helper.executeCommandH(token, "DEMOCRACYACTION", -1, -1)

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.America)
    println(l)
    assert(!(l contains Command.DEMOCRACYACTION))
    val tr1: TradeForCiv = numberofTradeH(gg, Civilization.America)
    println(tr1.trade)
    assert(tr1.trade + 6 == tr.trade)
    val tech = gg.playerDeck(Civilization.America).findPlayerTechnology(TechnologyName.Democracy).get
    println(tech.coins)
    assert(tech.coins == 1)
    val e1 = getCoins(gg, gg.playerDeck(Civilization.America))
    println(e1.coins)
    assert(e.coins + 1 == e1.coins)
  }

  test("Currency action, error resource already used") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME1.json", "test35/PLAY3.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    val param =
      """{"resource" : "Incense"}"""
    Helper.executeCommandH(token, "CURRENCYACTION", -1, -1, param)
  }

  test("Printing Press action") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME1.json", "test35/PLAY4.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    val cult = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    println(cult)
    assert(l contains Command.PRINTINGPRESSACTION)
    val e = getCoins(gg, gg.playerDeck(Civilization.America))
    println(e.coins)

    Helper.executeCommandH(token, "PRINTINGPRESSACTION", -1, -1)

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.America)
    println(l)
    assert(!(l contains Command.PRINTINGPRESSACTION))

    val cult1 = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    println(cult1)
    assert(cult1 + 5 == cult)
    val e1 = getCoins(gg, gg.playerDeck(Civilization.America))
    println(e1.coins)
    assert(e.coins + 1 == e1.coins)
    assert(gg.playerDeck(Civilization.America).findPlayerTechnology(TechnologyName.PrintingPress).get.coins == 1)

    // replacable part, stacking size
    val pl: PlayerLimits = getLimitsH(gg, Civilization.America)
    println(pl.stackinglimit)
    assert(pl.stackinglimit == 6)

  }


  test("Academy action") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME1.json", "test35/PLAY5.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    citiesForCivilization(gg, Civilization.America).foreach(println)
    val pr1: ProdForCity = getProductionForCityH(gg, Civilization.America, P(2, 2))
    println(pr1.prod)
    Helper.executeCommandH(token, "RESEARCH", -1, -1, "\"MilitaryScience\"")
    gg = I.getBoardForToken(token)
    val pr2: ProdForCity = getProductionForCityH(gg, Civilization.America, P(2, 2))
    println(pr2.prod)
    // increase production becuase of MilitaryScience
    assert(pr1.prod + 3 == pr2.prod)

  }

  test("Technologyu action again") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME1.json", "test35/PLAY6.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    // can use DEMOCRACYACTION
    assert(l contains Command.DEMOCRACYACTION)
  }

  test("Economy Victory") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME1.json", "test35/PLAY6.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    val coins = getCoins(gg, gg.playerDeck(Civilization.America))
    println(coins.coins)
    Helper.executeCommandH(token, "DEMOCRACYACTION", -1, -1)
    gg = I.getBoardForToken(token)
    val coins1 = getCoins(gg, gg.playerDeck(Civilization.America))
    println(coins1.coins)
    println(gg.endofgame)
    assert(gg.endofgame.isDefined && gg.endofgame.get.wintype == GameWinType.Economic)
  }

  test("Use Philosophy Action twice, errror") {
    val reg = Helper.ReadAndPlayForTwo("test35/BOARDGAME7.json", "test35/PLAY7.json", Civilization.Germany, Civilization.Arabs)
    val tokenG = reg._1
    val tokenA = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    var l = allowedCommandsH(gg, Civilization.Arabs)
    println(l)
    assert(!(l contains TechnologyName.Philosophy))
  }

  test("Military Victory") {
    val reg = Helper.ReadAndPlayForTwo("test35/BOARDGAME7.json", "test35/PLAY8.json", Civilization.Germany, Civilization.Arabs)
    val tokenG = reg._1
    val tokenA = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenG)
    println(gg.endofgame)
    assert(gg.endofgame.isEmpty)
    println(gg.battle)
    assert(gg.battle.isDefined)
//    {"command":"PLAYUNIT","civ":"Germany","p":{"row":0,"col":4},"param":null}
    Helper.executeCommandH(tokenG, "PLAYUNIT", 0, 4)
    gg = I.getBoardForToken(tokenG)
    println(gg.endofgame)
    assert(gg.endofgame.isDefined && gg.endofgame.get.wintype == GameWinType.Military)
    println(gg.battle)
    assert(gg.battle.isEmpty)

  }

  test("SPend trade for coin incorrect") {
    val reg = Helper.readBoardAndPlayT("test35/BOARDGAME9.json", "test35/PLAY9.json", Civilization.America)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val trade = numberofTradeH(gg,Civilization.America)
    println(trade.trade)
    // should be 21
    assert(trade.trade == 21)
  }

  }
