package civilization.test

import civilization.I
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.II
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue}
import Helper._


class Test27 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Check Currency") {

    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME1.json", "test27/PLAY1.json", Civilization.America)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
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
    val co = getCoins(gg, gg.playerDeck(Civilization.America))
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
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(l contains Command.KILLFIGURE)
    assert(l.length == 2)
    Helper.executeCommandH(token, "KILLFIGURE", -1, -1)
    gg = I.getBoardForToken(token)
    no = getNumberOfArmies(gg, Civilization.Spain)
    println(no)
    // only one army
    assert(no._1 == 1)
    // what's up now
    l = allowedCommandsH(gg, Civilization.Spain)
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

  test("Advance culture and iscard card") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME3.json", "test27/PLAY4.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l contains Command.DISCARDCARD)
    assert(!(l contains Command.ENDOFPHASE))
    assert(l.length == 1)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "DISCARDCARD")
    println(ite)
    Helper.executeCommandH(token, "DISCARDCARD", -1, -1,""" "Drought" """)

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.DISCARDCARD))
    assert(l contains Command.ENDOFPHASE)

    val cu = gg.cultureused.cards
    println(cu)
    // culture used
    assert(cu.length == 1)
  }

  test("Advance culture, get great person and resign put on map") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME5.json", "test27/PLAY5.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l.length == 2)
    assert(l contains Command.GREATPERSONPUTNOW)
    assert(l contains Command.GREATPERSONPUTNOWRESIGN)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "GREATPERSONPUTNOW")
    println(ite)
    Helper.executeCommandH(token, "GREATPERSONPUTNOWRESIGN", -1, -1)
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.GREATPERSONPUTNOW))
    assert(!(l contains Command.GREATPERSONPUTNOWRESIGN))
  }

  test("Advance culture, get great person and put on map") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME5.json", "test27/PLAY5.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    val c =
      """{"p":{"row":1,"col":2},"greatperson":"JimHenson"}"""
    Helper.executeCommandH(token, "GREATPERSONPUTNOW", 1, 1, c)
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.GREATPERSONPUTNOW))
    assert(!(l contains Command.GREATPERSONPUTNOWRESIGN))
    val co = getCoins(gg, gg.playerDeck(Civilization.China))
    println(co)
    assert(co.coins == 1)
    val s = toJ(II.getData(II.GETBOARDGAME, token))
    val ma = Helper.jmap(s)
    println(ma)
    var gtype: String = ""
    ma.value.foreach(t => {
      val ta: JsArray = t.as[JsArray]
      ta.value.foreach(m => {
        println(m)
        val ptype: Option[String] = (m \ "greatpersontype").asOpt[String]
        if (ptype.isDefined) gtype = ptype.get
      }
      )
    }
    )
    println(gtype)
    assert(gtype == "Humanitarian")
  }

  test("Buiding on water") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME6.json", "test27/PLAY6.json", Civilization.China)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    val l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l contains Command.BUYBUILDING)
    val ite: JsArray = toJ(II.getData(II.ITEMIZECOMMAND, token, "BUYBUILDING")).as[JsArray]
    println(ite)
    val ci = ite.value.head
    val li = (ci \ "list").as[JsArray]
    println(li)
    val ma = getSquare(gg, P(0, 1))
    println(ma)
    // (0,1) water, cannot build here
    assert(ma.terrain == Terrain.Water)
    li.value.foreach(t => {
      println(t)
      val p = (t \ "p").as[P]
      println(p)
      val ss = getSquare(gg, p)
      assert(ss.terrain != Terrain.Water)
    }
    )
  }

  test("Start of the game, great person") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME7.json", "test27/PLAY7.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l contains Command.GREATPERSONPUT)
    val ite: JsArray = toJ(II.getData(II.ITEMIZECOMMAND, token, "GREATPERSONPUT")).as[JsArray]
    println(ite)
    val c =
      """{"p":{"row":0,"col":0},"greatperson":"Valmiki"}"""
    Helper.executeCommandH(token, "GREATPERSONPUT", 1, 1, c)
    // command again
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.GREATPERSONPUT))
  }

  test("China handsize should be increased to 3") {
    val reg = Helper.ReadAndPlayForTwo("test27/BOARDGAME8.json", "test27/PLAY8.json", Civilization.China, Civilization.America)
    val tokenC = reg._1
    val tokenA = reg._2
    var gg = I.getBoardForToken(tokenC)
    val li = getLimitsH(gg, Civilization.China)
    println(li.handsize)
    assert(li.handsize == 3)
    val lia = getLimitsH(gg, Civilization.America)
    println(lia.handsize)
    assert(lia.handsize == 2)

  }

  test("Great General incresed the combat bonus") {
    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME9.json", "test27/PLAY9.json", Civilization.America)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val pl: PlayerLimits = getLimitsH(gg, Civilization.America)
    println(pl.combatBonus)
    assert(pl.combatBonus == 4)
  }
}

