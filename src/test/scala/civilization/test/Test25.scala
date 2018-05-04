package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard._
import civilization.gameboard.CultureTrack._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.helper._
import civilization.objects._
import play.api.libs.json.{JsArray, JsValue}
import civilization.io.fromjson._
import civilization.io.readdir.GameResources

class Test25 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("Test city creation by scout and army") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME1.json", "test25/PLAY1.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val num1 = getNumberOfArmies(gg, Civilization.China)
    println(num1)
    Helper.executeCommandH(token, "SETCITY", 4, 4)
    gg = I.getBoardForToken(token)
    val num2 = getNumberOfArmies(gg, Civilization.China)
    println(num2)
    // number of armiers should be the same
    assert(num1._1 == num2._1)
    // number of scouts should decrease by one
    assert(num1._2 == num2._2 + 1)

    //    {"command":"SETCITY","civ":"China","p":{"row":4,"col":4},"param":null}
  }

  test("Do not reuse scout after resource harvest") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME2.json", "test25/PLAY2.json", Civilization.China)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    val l = allowedCommands(gg, Civilization.China)
    println(l)
    // cannot SENDTRADE from scout already harvested
    assert(!(l contains Command.SENDPRODUCTION))
  }

  test("Do not reuse scout after send production") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME3.json", "test25/PLAY3.json", Civilization.China)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    val l = allowedCommands(gg, Civilization.China)
    println(l)
    assert(l contains Command.HARVESTRESOURCE)
    // itemize HARVESTRESOURCE
    val item: JsArray = toJ(AllowedCommands.itemizeCommandS(gg, Civilization.China, Command.HARVESTRESOURCE)).as[JsArray]
    println(item)
    var wasscout: Boolean = false
    item.value.foreach(e => {
      println(e)
      val pa = (e \ "param").as[JsValue]
      println(pa)
      val p: P = toP(pa)
      // no (2,0) scout, used already
      if (p == P(2, 0)) wasscout = true
    })
    assert(!wasscout)
  }

  test("Devout city to culture") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME4.json", "test25/PLAY4.json", Civilization.China)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    val l = allowedCommands(gg, Civilization.China)
    println(l)
    assert(l contains Command.DEVOUTTOCULTURE)
    val item: JsArray = toJ(AllowedCommands.itemizeCommandS(gg, Civilization.China, Command.DEVOUTTOCULTURE)).as[JsArray]
    println(item)

    val cu = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cu)
    assert(cu == 0)
    Helper.executeCommandH(token, "DEVOUTTOCULTURE", 1, 1, "[]")
    val gg1 = I.getBoardForToken(token)
    val cu1 = gg1.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cu1)
    // culture increased by 2
    assert(cu1 == 2)
    val l1 = allowedCommands(gg1, Civilization.China)
    println(l1)
    assert(!(l1 contains Command.DEVOUTTOCULTURE))
    val ss = II.getData(II.GETBOARDGAME, token)
    val j = Helper.jyou(toJ(ss))
    println(j)
    val res = (j \ "resources").as[JsArray]
    var resnum: Int = 0
    res.value.foreach(a => {
      println(a)
      val res = (a \ "resource").as[String]
      val num = (a \ "num").as[Int]
      if (res == "Culture") resnum = num
    })
    assert(resnum == 2)
  }

  test("Devout city to culture and scout") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME5.json", "test25/PLAY5.json", Civilization.China)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    val l = allowedCommands(gg, Civilization.China)
    println(l)
    assert(l contains Command.DEVOUTTOCULTURE)
    val j = AllowedCommands.itemizeCommandS(gg, Civilization.China, Command.DEVOUTTOCULTURE)
    // list with scout 0,0
    println(j)
    Helper.executeCommandFail(token, "DEVOUTTOCULTURE", 2, 2,"""[{"row" : 0, "col" : 1}]""")
    Helper.executeCommandH(token, "DEVOUTTOCULTURE", 2, 2,"""[{"row" : 0, "col" : 0}]""")
    val gg1 = I.getBoardForToken(token)
    val cu1 = gg1.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cu1)
    // culture increased by 2
    assert(cu1 == 2)
  }

  test("Test culture track") {
    println("Test gen")
    val gr: GameResources = GameResources.instance()
    assert(!gr.civ.isEmpty)
    assert(!gr.culturetrack.isEmpty)
    assert(gr.culturetrack.length == 3)
    gr.culturetrack.foreach(c => {
      val se: CultureTrackSegment = c
      println(se)
      se.greatperson.foreach(println)
    })
  }

  test("Test game resourece and culture track") {
    val s = II.getData(II.LISTOFRES)
    val j = toJ(s)
    println(j)
    val c = (j \ "culturetrack").as[JsArray]
    println(c)
    c.value.foreach(println)
  }


  test("Spend culture to advance") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME6.json", "test25/PLAY6.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.China)
    println(l)
    assert(l contains Command.ADVANCECULTURE)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "ADVANCECULTURE")
    println(ite)
    val item: JsArray = toJ(ite).as[JsArray]
    assert(item.value.length == 1)
    Helper.executeCommandH(token, "ADVANCECULTURE", -1, -1)
    gg = I.getBoardForToken(token)
    l = allowedCommands(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.ADVANCECULTURE))
    val cu1 = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cu1)
    // culture spend
    assert(cu1 == 0)
    // trade
    val t = numberofTrade(gg, Civilization.China)
    println(t)
    assert(t.trade == 3)
    val ss = II.getData(II.GETBOARDGAME, token)
    val j = Helper.jyou(toJ(ss))
    println(j)
    val culprogress: Int = (j \ "cultureprogress").as[Int]
    println(culprogress)
    assert(culprogress == 1)
  }

  test("Spend culture to advance next level") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME6.json", "test25/PLAY7.json", Civilization.China)
    val token = reg._1
    var ite = II.getData(II.ITEMIZECOMMAND, token, "ADVANCECULTURE")
    println(ite)
    val item: JsArray = toJ(ite).as[JsArray]
    assert(item.value.length == 1)
    item.value.foreach(c => {
      println(c)
    })
    var gg = I.getBoardForToken(token)
    val cu1 = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cu1)
    assert(cu1 == 6)
    val tra = numberofTrade(gg, Civilization.China)
    println(tra)
    Helper.executeCommandH(token, "ADVANCECULTURE", -1, -1)
    gg = I.getBoardForToken(token)
    val cu2 = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cu2)
    assert(cu2 == 1)
    val tra1 = numberofTrade(gg, Civilization.China)
    println(tra1)
    assert(tra1.spendOnCulture == 3)
    assert(tra1.trade == 7)
  }

  test("Implemet TradingPost research") {
    val reg = Helper.readBoardAndPlayT("test25/BOARDGAME8.json", "test25/PLAY8.json", Civilization.China)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    val co = getCoins(gg,Civilization.China)
    println(co)
    assert(co.coins == 1)
    gg.journal.foreach(println)
    assert(gg.journal.nonEmpty)
  }

}
