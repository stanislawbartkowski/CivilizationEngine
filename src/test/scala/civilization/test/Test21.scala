package civilization.test

import civilization.I
import civilization.gameboard.GameBoard
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.objects._
import play.api.libs.json._
import civilization.io.fromjson.{toJ, _}
import civilization.helper._
import Helper._

class Test21 extends FunSuite with ImplicitMiximFromJson {

  Helper.I

  test("Building") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME1.json", "test21/PLAY1.json", Civilization.America)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    assert(l.find(_ == Command.BUYBUILDING).isDefined)
    val s = II.itemizeCommand(token, "BUYBUILDING")
    val j: JsArray = toJ(s).as[JsArray]
    println(j)
    assert(1 == j.value.length)
    val jj: JsValue = j.value(0)
    println(jj)
    val li: JsArray = (jj \ "list").as[JsArray]
    println(li)
    li.value.foreach(
      e => {
        val b = (e \ "building").as[String]
        assert(b == "Market" || b == "Temple")
      }
    )
    val beforeb = gg.market.buildings.noB(BuildingName.Temple)
    val prodbefore: ProdForCity = getProductionForCityH(gg, Civilization.America, P(1, 5))
    // buy building
    val c =
      """{"p":{"row":0,"col":4},"building":"Temple"}"""
    Helper.executeCommandH(token, "BUYBUILDING", 1, 5, c)
    // commands
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.America)
    println(l)
    // no more building
    assert(l.find(_ == Command.BUYBUILDING).isEmpty)
    val ma: MapSquareP = getSquare(gg, P(0, 4))
    println(ma)
    // Temple
    assert(BuildingName.Temple == ma.s.building.get.name)
    val afterb = gg.market.buildings.noB(BuildingName.Temple)
    // compare number of buildings before and after
    println(beforeb)
    println(afterb)
    assert(beforeb - 1 == afterb)
    // trade
    val prodafter: ProdForCity = getProductionForCityH(gg, Civilization.America, P(1, 5))
    println(prodbefore)
    println(prodafter)
    // temple: no production
    assert(prodbefore.prod - 2 == prodafter.prod)
    val ss = II.getData(II.GETBOARDGAME, token)
    println(ss)
    val map: JsArray = (toJ(ss) \ "board" \ "map").as[JsArray]
    println(ma)
    var bfound: String = null
    map.value.foreach(p => {
      val a: JsArray = p.as[JsArray];
      a.value.foreach(e => {
        val s = (e \ "building").asOpt[String].orElse(null)
        if (s != null) bfound = s.get
      }
      )
    })
    println(bfound)
    assert("Temple" == bfound)
  }

  test("Building replace") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME2.json", "test21/PLAY2.json", Civilization.America)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    // check 2,4 building should be removed
    val mab: MapSquareP = getSquare(gg, P(2, 4))
    println(mab)
    assert(mab.s.building.isDefined)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    assert(l.find(_ == Command.BUYBUILDING).isDefined)
    val s = II.itemizeCommand(token, "BUYBUILDING")
    val j = toJ(s).as[JsArray].value(0)
    //    println(j)
    val li: JsArray = (j \ "list").as[JsArray]
    li.value.foreach(e => {
      println(e)
      val la = (e \ "list").as[JsArray]
      assert(1 == la.value.length)
      val p = toP(la.value(0))
      println(p)
      assert(p == P(2, 4))
    }
    )
    val beforeb = gg.market.buildings.noB(BuildingName.Market)

    // build replacing previous star
    val c =
      """{"p":{"row":0,"col":4},"building":"Temple"}"""
    Helper.executeCommandH(token, "BUYBUILDING", 1, 5, c)
    gg = I.getBoardForToken(token)
    // check 0,4, temple should be built
    val ma: MapSquareP = getSquare(gg, P(0, 4))
    println(ma)
    assert(ma.s.building.isDefined)
    assert(ma.s.building.get.name == BuildingName.Temple)
    // check 2,4 building should be removed
    val ma1: MapSquareP = getSquare(gg, P(2, 4))
    println(ma1)
    assert(ma1.s.building.isEmpty)
    val afterb = gg.market.buildings.noB(BuildingName.Market)
    // market released to market again
    assert(beforeb + 1 == afterb)
  }


  test("Building again") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME3.json", "test21/PLAY3.json", Civilization.America)
    val token: String = reg._1
    var gg: GameBoard = reg._2
    val ss = getSquare(gg, P(1, 1))
    println(ss)
    val s = II.itemizeCommand(token, "BUYBUILDING")
    val j = toJ(s).as[JsArray].value(0)
    //    println(j)
    val li: JsArray = (j \ "list").as[JsArray]
    assert(!li.value.isEmpty)
    li.value.foreach(e => {
      println(e)
      val ee = (e \ "building").as[String]
      println(ee)
      // only markets, no tradingpost
      assert(ee == "Market")
    }
    )
  }

  test("Next level of discoveries") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME4.json", "test21/PLAY4.json", Civilization.Russia)
    val token: String = reg._1
    var gg: GameBoard = reg._2
    val l = allowedCommandsH(gg, Civilization.Russia)
    println(l)
    val s = II.itemizeCommand(token, "RESEARCH")
    println(s)
    Helper.executeCommandH(token, "RESEARCH", -1, -1, "\"Construction\"")
  }

  test("Discoveries, something wrong") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME5.json", "test21/PLAY5.json", Civilization.America)
    val token: String = reg._1
    var gg: GameBoard = reg._2
    // sounds
    var no = gg.market.buildings.noB(BuildingName.University)
    println(no)
    val s: String = II.getData(II.GETBOARDGAME, token)
    println(s)
    // was broken for this data
  }

  test("Start move, double figures") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME6.json", "test21/PLAY6.json", Civilization.Egypt)
    val token: String = reg._1
    var gg: GameBoard = reg._2
    val m : MapSquareP = getSquare(gg,P(4,3))
    println(m.s.figures)
    assert(m.s.figures.numberofArmies == 2)
    Helper.executeCommandH(token, "STARTMOVE", 4, 3, "{ \"numberofArmies\" : 1, \"numberofScouts\" : 0}")

  }
}