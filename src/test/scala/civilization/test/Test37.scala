package civilization.test

import civilization.gameboard._
import civilization.helper._
import civilization.helper.battle.BattleActions
import civilization.io.fromjson._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper._
import org.scalatest.FunSuite
import civilization.I
import play.api.libs.json.{JsArray, JsNull, JsValue}


class Test37 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Writing technology") {
    val reg = Helper.ReadAndPlayForTwo("test37/BOARDGAME1.json", "test37/PLAY1.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    Helper.executeCommandH(tokenA, "BUYMOUNTED", 2, 2)
    var gg: GameBoard = I.getBoardForToken(tokenA)
    println(gg.isSuspended)
    // check the last command
    val lastC = gg.play.commands.reverse.head
    println(lastC.status)
    assert(lastC.status == CommandStatus.Su)
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    assert(l.isEmpty)
    l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l.isEmpty)
    gg.susplist.foreach(println)
    // you
    val b = toJ(II.getData(II.GETBOARDGAME, tokenC))
    val y = jyou(b)
    println(y)
    val susp = (y \ "suspended").as[JsValue]
    println(susp)
    assert(susp != JsNull)
    val co = (susp \ "command").as[JsValue]
    println(co)
    val a: JsArray = (susp \ "list").as[JsArray]
    println(a.value.length)
    assert(a.value.length == 1)
    // opponent
    val b1 = toJ(II.getData(II.GETBOARDGAME, tokenA))
    val y1 = jyou(b1)
    println(y1)
    val susp1 = (y1 \ "suspended").as[JsValue]
    println(susp1)
    assert(susp1 == JsNull)
  }

  test("Writing technology, letgo") {
    val reg = Helper.ReadAndPlayForTwo("test37/BOARDGAME1.json", "test37/PLAY1.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    val mounted = numof(gg, Civilization.America, CombatUnitType.Mounted)
    println(mounted)
    Helper.executeCommandH(tokenA, "BUYMOUNTED", 2, 2)
    gg = I.getBoardForToken(tokenA)
    val mounted1 = numof(gg, Civilization.America, CombatUnitType.Mounted)
    // suspended here
    assert(mounted == mounted1)

    Helper.executeCommandH(tokenC, "LETSUSPENDEDGO")
    gg = I.getBoardForToken(tokenA)
    val mounted2 = numof(gg, Civilization.America, CombatUnitType.Mounted)
    println(mounted2)
    assert(mounted + 1 == mounted2)
    assert(!gg.isSuspended)
  }

}