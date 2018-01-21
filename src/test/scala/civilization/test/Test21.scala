package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.objects._
import play.api.libs.json._
import civilization.io.fromjson.{toJ, _}
import civilization.helper._

class Test21 extends FunSuite with ImplicitMiximFromJson {

  Helper.I

  test("Building") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME1.json", "test21/PLAY1.json", Civilization.America)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.America)
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
    val prodbefore: ProdForCity = getProductionForCity(gg, Civilization.America, P(1,5))
    // buy building
    val c =
      """{"p":{"row":0,"col":4},"building":"Temple"}"""
    Helper.executeCommandH(token, "BUYBUILDING", 1, 5, c)
    // commands
    gg = I.getBoardForToken(token)
    l = allowedCommands(gg, Civilization.America)
    println(l)
    // no more building
    assert(l.find(_ == Command.BUYBUILDING).isEmpty)
    val ma : MapSquareP = getSquare(gg,P(0,4))
    println(ma)
    // Temple
    assert(BuildingName.Temple == ma.s.building.get.name)
    val afterb = gg.market.buildings.noB(BuildingName.Temple)
    // compare number of buildings before and after
    println(beforeb)
    println(afterb)
    assert(beforeb -1 == afterb)
    // trade
    val prodafter: ProdForCity = getProductionForCity(gg, Civilization.America, P(1,5))
    println(prodbefore)
    println(prodafter)
    // temple: no production
    assert(prodbefore.prod -2 == prodafter.prod)
    val ss = II.getData(II.GETBOARDGAME,token)
    println(ss)
    val map: JsArray = (toJ(ss) \ "board" \ "map").as[JsArray]
    println(ma)
    var bfound : String = null
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
}
