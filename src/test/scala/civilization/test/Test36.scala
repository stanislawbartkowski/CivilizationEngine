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
import civilization.io.readdir.GameResources

class Test36 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Great person, Henry Fond") {
    val reg = Helper.readBoardAndPlayT("test36/BOARDGAME1.json", "test36/PLAY1.json", Civilization.Rome)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val pl : PlayerLimits = getLimitsH(gg,Civilization.Rome)
    println(pl.travelSpeed)
    // HenryFord is increasing speed by one
    assert(pl.travelSpeed == 3)
  }
}