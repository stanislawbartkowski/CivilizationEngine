package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.gameboard._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir._
import civilization.objects._
import org.scalatest.FunSuite
import play.api.libs.json._
import civilization.io.fromjson.{toJ, _}
import civilization.io.readdir.GenBoard.genBoard
import civilization.test.Helper._



class Test24 extends FunSuite with ImplicitMiximFromJson  {

  Helper.I

  test("Check technology action") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME1.json", "test24/PLAY1.json", Civilization.Spain)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Spain)
    println(l)
    assert(l.find(_ == Command.POTTERYACTION).isDefined)
    assert(l.find(_ == Command.PHILOSOPHYACTION).isEmpty)
    val ss = II.itemizeCommand(token,"POTTERYACTION")
    println(ss)
    val j : JsArray = toJ(ss).as[JsArray]
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
    Helper.executeCommandH(token, "POTTERYACTION", 1, 2, param)
    var gg = I.getBoardForToken(token)
    println(getCoins(gg,Civilization.Spain))
    // check number of coins
    j = Helper.getB(token)
    y = jyou(j)
    println(y)
    val cc = (y \ "coins").as[Int]
    println(cc)
    assert(c + 1 == cc)
    // now check technology, coin on Pottery
    val t : JsArray = (y \ "tech").as[JsArray]
//    println(t)
    var coinFound = false
    t.value.foreach( tt => {
      val tech = (tt \ "tech").as[String]
      val coin = (tt \ "coins").as[Int]
      if (tech == "Pottery") coinFound = coin == 1
    })
    assert(coinFound)
    // check resource
    val r : JsArray = (y \ "resources").as[JsArray]
    r.value.foreach( r => {
      println(r)
      val num :Int = (r \ "num").as[Int]
      // no resource
      assert(num == 0)
    })
    // check resource on board
    //println(j)
    val res : JsArray = (j \ "board" \ "resources").as[JsArray]
    //println(res)
    res.value.foreach( r => {
      println(r)
      val na = (r \ "resource").as[String]
      val num :Int = (r \ "num").as[Int]
      if (na == "Wheat" || na == "Incense") assert(num == 1)
    })
  }

  test("Try to  execute for hutvillages") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME2.json", "test24/PLAY2.json", Civilization.Rome)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Rome)
    println(l)
    val param = """[{"hv":"Hut","resource" : "Incense"},{"hv":"Hut","resource" : "Wheat"},{"resource" : "Iron"}]"""
    Helper.executeCommandH(token, "POTTERYACTION", 1, 2, param)
    val j = Helper.getB(token)
    val y = jyou(j)
    println(y)
    val hv : JsArray = (y \ "hutvillages" \ "list").as[JsArray]
    // all huts are used
    assert(hv.value.isEmpty)
    val res : JsArray = (j \ "board" \ "resources").as[JsArray]
    //println(res)
    res.value.foreach( r => {
      println(r)
      val na = (r \ "resource").as[String]
      val num :Int = (r \ "num").as[Int]
      if (na == "Wheat" || na == "Incense" || na=="Iron") assert(num == 1)
    })
//    println(j)
    val hvu : JsValue = (j \ "board" \ "hutvillagesused").as[JsValue]
    println(hvu)
    val num = (hvu \ "Hut").as[Int]
    assert(num == 2)
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
    val token: String = reg._1
    val j = Helper.getB(token)
    val y = jyou(j)
    println(y)
    val co = (y \ "coins").as[Int]
    println(co)
    assert(co == 1)
  }

  test("Repeated technology resource action") {
    val reg = Helper.readBoardAndPlayT("test24/BOARDGAME2.json", "test24/PLAY5.json", Civilization.Rome)
    val token: String = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Rome)
    println(l)
    // pottery action only once
    assert(l.find(_ == Command.POTTERYACTION).isEmpty)
    assert(l.find(_ == Command.BUYARMY).isDefined)
  }
  }
