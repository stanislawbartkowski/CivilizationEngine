package civilization.test

import civilization.I
import civilization.I.executeCommand
import civilization.gameboard.{GameBoard, WinnerLoot}
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._
import civilization.helper.battle.BattleActions


class Test34 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Construction action") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME1.json", "test34/PLAY1.json", Civilization.China)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val s : Seq[P] = CityAvailableForAction(gg,Civilization.China)
    println(s)
    // CONSTRUCIONACTION, does not block city action
    assert(s.length == 3)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.CONSTRUCTIONACTION))
  }

  test("Culture victory") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME1.json", "test34/PLAY2.json", Civilization.China)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    Helper.executeCommandH(token, "ADVANCECULTURE",-1,-1)
    gg  = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.China)
    println(l)
    // end of game
    assert(l.isEmpty)
    assert(gg.endofgame.isDefined)
    println(gg.endofgame.get)
    assert(gg.endofgame.get.winner == Civilization.China)
    assert(gg.endofgame.get.wintype == GameWinType.Culture)
    val b = toJ(II.getData(II.GETBOARDGAME,token))
    println(b)
    val e = (b \ "board" \ "endofgame")
    println(e)
    val c1 = (e \ "winner").as[String]
    val c2 = (e \ "wintype").as[String]
    assert(c1 == "China")
    assert(c2 == "Culture")
  }


}