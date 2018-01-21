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

class Test21 extends FunSuite with ImplicitMiximFromJson {

  Helper.I

  test("Building") {
    val reg = Helper.readBoardAndPlayT("test21/BOARDGAME1.json", "test21/PLAY1.json", Civilization.America)
    val token: String = reg._1
    val gg: GameBoard = I.getBoardForToken(token)
    val l = allowedCommands(gg, Civilization.America)
    println(l)
    assert(l.find(_ == Command.BUYBUILDING).isDefined)
    val s = II.itemizeCommand(token,"BUYBUILDING")
    val j : JsArray = toJ(s).as[JsArray]
    println(j)
    assert(1 == j.value.length)
    val jj : JsValue = j.value(0)
    println(jj)
    val li : JsArray = (jj \ "list").as[JsArray]
    println(li)
    li.value.foreach(
      e => {
        val b = (e \ "building").as[String]
        assert(b == "Market" || b == "Temple")
      }
    )

  }
}
