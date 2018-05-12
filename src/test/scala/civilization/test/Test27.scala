package civilization.test

import civilization.I
import civilization.I.{II, executeCommand}
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson.toJ
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.objects.Command
import civilization.objects._
import play.api.libs.json.{JsArray, JsValue}


class Test27 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("Check Currency") {

    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME1.json", "test27/PLAY1.json", Civilization.America)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.America)
    println(l)
    assert(l contains Command.CURRENCYACTION)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "CURRENCYACTION")
    println(ite)
    // run command
    val param =
      """{"resource" : "Incense"}"""
    Helper.executeCommandH(token, "CURRENCYACTION", 2, 2, param)
    // check culture
    gg = I.getBoardForToken(token)
    val culture = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    println(culture)
    assert(culture == 3)
    // incense, 0
    val incense = gg.playerDeck(Civilization.America).resou.nof(Resource.Incense)
    println(incense)
    assert(incense == 0)
    val co = getCoins(gg, Civilization.America)
    println(co)
    assert(co.coins == 0)
  }

  test("Test great persons type as resource") {
    println("Test wonders")
    val s: String = II.getData(II.LISTOFRES)
    val j: JsValue = toJ(s)
    val w: JsArray = (j \ "greatpersontype").as[JsArray]
    println(w)
    w.value.foreach(println)
    val w1: JsArray = (j \ "greatperson").as[JsArray]
    w1.value.foreach(println)
    val w2: JsArray = (j \ "cards").as[JsArray]
    w2.value.foreach(println)
  }

  test("Movement stopped in water") {

    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME2.json", "test27/PLAY2.json", Civilization.Spain)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var no = getNumberOfArmies(gg, Civilization.Spain)
    println(no)
    assert(no._1 == 2)
    var l = allowedCommands(gg, Civilization.Spain)
    println(l)
    assert(l contains Command.KILLFIGURE)
    assert(l.length == 1)
    Helper.executeCommandH(token, "KILLFIGURE", -1, -1)
    gg = I.getBoardForToken(token)
    no = getNumberOfArmies(gg, Civilization.Spain)
    println(no)
    // only one army
    assert(no._1 == 1)
    // what's up now
    l = allowedCommands(gg, Civilization.Spain)
    println(l)
    assert(l contains Command.ENDOFPHASE)
    assert(!(l contains Command.KILLFIGURE))
  }

  test("Advance cultuer and get great person") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME3.json", "test27/PLAY3.json", Civilization.China)
    val token = reg._1
    var ite = II.getData(II.ITEMIZECOMMAND, token, "ADVANCECULTURE")
    var gg = I.getBoardForToken(token)
    var ccards = gg.playerDeck(Civilization.China).cultureresource.cards
    assert(ccards.length == 2)
    println(ite)
    Helper.executeCommandH(token, "ADVANCECULTURE", -1, -1)
    gg = I.getBoardForToken(token)
    ccards = gg.playerDeck(Civilization.China).cultureresource.cards
    // number of culture cards is the same
    assert(ccards.length == 2)
    val gper = gg.playerDeck(Civilization.China).cultureresource.persons
    println(gper)
    // one great person
    assert(gper.length == 1)
  }

  test("Advance cultuer and discard card") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME3.json", "test27/PLAY4.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.China)
    println(l)
    assert(l contains Command.DISCARDCARD)
    assert(!(l contains Command.ENDOFPHASE))
    assert(l.length == 1)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "DISCARDCARD")
    println(ite)
    Helper.executeCommandH(token, "DISCARDCARD", -1, -1,""" "Drought" """)

    gg = I.getBoardForToken(token)
    l = allowedCommands(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.DISCARDCARD))
    assert(l contains Command.ENDOFPHASE)

    val cu = gg.cultureused.cards
    println(cu)
    // culture used
    assert(cu.length == 1)
  }


  }
