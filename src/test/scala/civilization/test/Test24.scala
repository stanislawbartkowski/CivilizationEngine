package civilization.test

import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson.{ImplicitMiximFromJson, toJ, _}
import civilization.objects._
import civilization.test.Helper.{II, _}
import org.scalatest.FunSuite
import play.api.libs.json._
import civilization.I



class Test24 extends FunSuite with ImplicitMiximFromJson {

  Helper.I

  test("Check technology action") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME1.json", "test24/PLAY1.json", Civilization.Spain)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(l.find(_ == Command.POTTERYACTION).isDefined)
    assert(l.find(_ == Command.PHILOSOPHYACTION).isEmpty)
    val ss = II.itemizeCommand(token, "POTTERYACTION")
    println(ss)
    val j: JsArray = toJ(ss).as[JsArray]
    assert(!j.value.isEmpty)
  }

  test("Try to  execute") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME1.json", "test24/PLAY1.json", Civilization.Spain)
    val token = reg._1
    // number of coins before
    var j = Helper.getB(token)
    var y = jyou(j)
    val c = (y \ "coins").as[Int]
    println(c)

    val param = """[{"resource" : "Incense"},{"resource" : "Wheat"}]"""
    Helper.executeCommandH(token, "POTTERYACTION", -1, -1, param)
    var gg : GameBoard = I.getBoardForToken(token)
    println(getCoins(gg, gg.playerDeck(Civilization.Spain)))
    // check number of coins
    j = Helper.getB(token)
    y = jyou(j)
    println(y)
    val cc = (y \ "coins").as[Int]
    println(cc)
    assert(c + 1 == cc)
    // now check technology, coin on Pottery
    val t: JsArray = (y \ "tech").as[JsArray]
    //    println(t)
    var coinFound = false
    t.value.foreach(tt => {
      val tech = (tt \ "tech").as[String]
      val coin = (tt \ "coins").as[Int]
      if (tech == "Pottery") coinFound = coin == 1
    })
    assert(coinFound)
    // check resource
    val r: JsArray = (y \ "resources").as[JsArray]
    r.value.foreach(r => {
      println(r)
      val num: Int = (r \ "num").as[Int]
      // no resource
      assert(num == 0)
    })
    // check resource on board
    //println(j)
    val res: JsArray = (j \ "board" \ "resources").as[JsArray]
    //println(res)
    res.value.foreach(r => {
      println(r)
      val na = (r \ "resource").as[String]
      val num: Int = (r \ "num").as[Int]
      if (na == "Wheat" || na == "Incense") assert(num == 1)
    })
  }

  test("Try to  execute for hutvillages") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME2.json", "test24/PLAY2.json", Civilization.Rome)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    val param = """[{"hv":"Hut","resource" : "Incense"},{"hv":"Hut","resource" : "Wheat"},{"resource" : "Iron"}]"""
    Helper.executeCommandH(token, "POTTERYACTION", 1, 2, param)
    val j = Helper.getB(token)
    val y = jyou(j)
    println(y)
    val hv: JsArray = (y \ "hutvillages" \ "list").as[JsArray]
    // all huts are used
    assert(hv.value.isEmpty)
    val res: JsArray = (j \ "board" \ "resources").as[JsArray]
    //println(res)
    res.value.foreach(r => {
      println(r)
      val na = (r \ "resource").as[String]
      val num: Int = (r \ "num").as[Int]
      if (na == "Wheat" || na == "Incense" || na == "Iron") assert(num == 1)
    })
    //    println(j)
    val hvu: JsValue = (j \ "board" \ "hutvillagesused").as[JsValue]
    println(hvu)
    val num = (hvu \ "Hut").as[Int]
    assert(num == 2)
    // try again
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    assert(!(l contains Command.POTTERYACTION))
  }

  test("Verify combat bonus, two barracks built") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME2.json", "test24/PLAY3.json", Civilization.Rome)
    val token: String = reg._1
    val j = Helper.getB(token)
    val y = jyou(j)
    println(y)
    val bonus = (y \ "combatbonus").as[Int]
    println(bonus)
    assert(bonus == 4)
  }

  test("Verify combat bonus, RailRoad, additional coin") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME2.json", "test24/PLAY4.json", Civilization.Rome)
    // Rome has Code of Low initially
    val token: String = reg._1
    var gg : GameBoard = I.getBoardForToken(token)
    val coi = getCoins(gg,gg.playerDeck(Civilization.Rome))
    println(coi)
    val j = Helper.getB(token)
    val y = jyou(j)
    println(y)
    val co = (y \ "coins").as[Int]
    println(co)
    assert(co == 2)
  }

  test("Repeated technology resource action") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME2.json", "test24/PLAY5.json", Civilization.Rome)
    val token: String = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    // pottery action only once
    assert(l.find(_ == Command.POTTERYACTION).isEmpty)
    assert(l.find(_ == Command.BUYARMY).isDefined)
  }

  test("Test spend trade") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME6.json", "test24/PLAY6.json", Civilization.Egypt)
    val token: String = reg._1
    val gg: GameBoard = I.getBoardForToken(token)
    val trade = numberofTradeH(gg, Civilization.Egypt)
    println(trade)
    println(trade.trade)
    assert(trade.trade == 2)
    assert(trade.toprod == 3)
  }

  test("Check culture is exported for square") {
//    val token: String = II.getData(II.REGISTEROWNER, "China")
    val token : String = registerOwner("China")
    println(token)
    val gg: GameBoard = I.getBoardForToken(token)
    val seq = allSquares(gg)
    var culture: Int = 0
    seq.foreach(s => {
      println(s)
      if (s.revealed && s.resource.isDefined && s.resource.get == Resource.Culture) culture = culture + 1
    })
    println(culture)
    assert(culture == 1)

    val s = II.getData(II.GETBOARDGAME, token)
    println(s)
    val j = toJ(s)
    val ma = jmap(j)
    var mculture: Int = 0
    ma.value.foreach(t => {
      val a: JsArray = t.as[JsArray]
      //      println(a)
      a.value.foreach(s => {
        println(s)
        val re = (s \ "resource").asOpt[Resource.T]
        if (re.isDefined && re.get != null) {
          println(re.get)
          if (re.get == Resource.Culture) mculture = mculture + 1
        }
      })
    })
    println(mculture)
    assert(mculture == 1)
  }

  test("Gather culture for city") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME7.json", "test24/PLAY7.json", Civilization.China)
    val token = reg._1
    println(token)
    val gg: GameBoard = I.getBoardForToken(token)
    val cul = cultureForCity(gg, P(1, 1))
    println(cul)
    assert(cul.culture == 2)
    val s = II.getData(II.GETBOARDGAME, token)
    val j = toJ(s)
    val ma = jmap(j)
    var was2 : Boolean = false
    ma.value.foreach(t => {
      val a: JsArray = t.as[JsArray]
      //      println(a)
      a.value.foreach(s => {
        println(s)
        val cul : Int = (s \ "culture").as[Int]
        println(cul)
        if (cul == 2) was2 = true
      })
    })
    assert(was2)
  }

  test("Gather culture for city and from building") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME7.json", "test24/PLAY8.json", Civilization.China)
    val token = reg._1
    println(token)
    val gg: GameBoard = I.getBoardForToken(token)
    // Library is built
    val s = getSquare(gg, P(2,2))
    println(s)
    println(s.culture)
    assert(s.culture == 1)
    val cul = cultureForCity(gg, P(1, 1))
    println(cul)
    assert(cul.culture == 3)
  }
  }
