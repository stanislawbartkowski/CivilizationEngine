package civilization.test

import civilization.I
import civilization.I.executeCommand
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._


class Test32 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Philosopy action") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME1.json", "test32/PLAY1.json", Civilization.Arabs)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Arabs)
    println(l)
    assert(l contains Command.PHILOSOPHYACTION)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "PHILOSOPHYACTION")
    println(ite)
    // execute
    val param =
      """[{"resource" : "Incense"},{"resource" : "Wheat"},{"resource":"Iron"}]"""
    Helper.executeCommandH(token, "PHILOSOPHYACTION", 2, 2, param)
    gg = I.getBoardForToken(token)
    val gp = gg.playerDeck(Civilization.Arabs).cultureresource.persons
    // great person taken
    assert(gp.length == 1)
  }

  test("Construction action") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME2.json", "test32/PLAY2.json", Civilization.Rome)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    assert(l contains Command.CONSTRUCTIONACTION)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "CONSTRUCTIONACTION")
    println(ite)
    val param =
      """{"resource" : "Wheat"}"""
    Helper.executeCommandH(token, "CONSTRUCTIONACTION", 1, 1, param)
    gg = I.getBoardForToken(token)
    val prod = getProductionForCityH(gg, Civilization.Rome, P(1, 1))
    println(prod)
    assert(prod.fromwheat == 5)
    assert(prod.prod == 12)
  }

  test("MetalCasting action") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME3.json", "test32/PLAY3.json", Civilization.China)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    val cul = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cul)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "METALCASTINGACTION")
    println(ite)
    val param =
      """{"resource" : "Incense"}"""
    Helper.executeCommandH(token, "METALCASTINGACTION", 1, 6, param)
    gg = I.getBoardForToken(token)
    val cul1 = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cul1)
    // increase by 7
    assert(cul1 == 18)
    val ince = gg.playerDeck(Civilization.China).resou.nof(Resource.Incense)
    println(ince)
    assert(ince == 0)
  }

  test("Banking action") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME4.json", "test32/PLAY4.json", Civilization.Germany)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Germany)
    println(l)
    assert(l contains Command.BANKINGACTION)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "BANKINGACTION")
    println(ite)
    val param =
      """{"resource" : "Wheat"}"""
    Helper.executeCommandH(token, "BANKINGACTION", 6, 2, param)
    gg = I.getBoardForToken(token)
    val prod = getProductionForCityH(gg, Civilization.Germany, P(6, 2))
    println(prod)
    assert(prod.fromwheat == 7)
    assert(prod.prod == 12)

    val co = getCoins(gg, gg.playerDeck(Civilization.Germany))
    println(co)
    // coin from Bank
    assert(co.coins == 1)

    val trade = numberofTradeH(gg, Civilization.Germany)
    println(trade)
  }

  test("ChichenItza wonder") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME5.json", "test32/PLAY5.json", Civilization.Germany)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val prod = getProductionForCityH(gg, Civilization.Germany, P(2, 2))
    println(prod)
    assert(prod.fromwonders == 3)
    assert(prod.prod == 13)
  }

  test("Collosuss wonder") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME6.json", "test32/PLAY6.json", Civilization.Russia)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val trade = numberofTradeH(gg, Civilization.Russia)
    println(trade)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """ "Research" """)
    gg = I.getBoardForToken(token)
    val trade1 = numberofTradeH(gg, Civilization.Russia)
    println(trade1)
    assert(trade1.begofturn == 3)
    assert(trade1.trade == 3)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """ "StartOfTurn" """)
    gg = I.getBoardForToken(token)
    val trade2 = numberofTradeH(gg, Civilization.Russia)
    println(trade2)

  }

  test("Sailing technology") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME7.json", "test32/PLAY7.json", Civilization.Arabs)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val li: PlayerLimits = getLimits(gg, gg.playerDeck(Civilization.Arabs))
    println(li)
    println(li.travelSpeed)
    assert(li.travelSpeed == 4)
    assert(li.waterstopallowed)
    assert(li.watercrossingallowed)
  }

  test("Navy technology") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME8.json", "test32/PLAY8.json", Civilization.Spain)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "BUYARMY")
    println(ite)
    val lp = toJ(ite).as[JsArray]
    println(lp)
    var exist: Boolean = false
    lp.value.foreach(e => {
      val pp = (e \ "p").as[P]
      println(pp)
      assert(pp == P(2, 2))
      val pa = (e \ "param").as[P]
      if (pa == P(4, 6)) exist = true
    }
    )
    assert(exist)
    Helper.executeCommandH(token, "BUYARMY", 2, 2, """ { "row" : 4,"col" : 6} """)
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    val s = getSquare(gg, P(4, 6))
    println(s)
    assert(s.s.figures.numberofArmies == 1)
  }

  test("Something strange, free wonder") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME8.json", "test32/PLAY9.json", Civilization.Spain)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val d = gg.playerDeck(Civilization.Spain)
    println(d.freeWonder)
    assert(d.freeWonder.isEmpty)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(!(l contains Command.FREEWONDER))
    val de = gg.playerDeck(Civilization.Spain)
    println(de.freeWonder)
    assert(de.freeWonder.isEmpty)
  }

  test("Something strange, wonder not appropraite") {
    val reg = Helper.readBoardAndPlayT("test32/BOARDGAME10.json", "test32/PLAY10.json", Civilization.Egypt)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    val prod = getProductionForCityH(gg,Civilization.Egypt,P(2,2))
    println(prod.prod)
    assert(prod.prod == 8)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "BUYWONDER")
    println(ite)
  }
  }

