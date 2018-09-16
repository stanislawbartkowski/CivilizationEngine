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
import civilization.action.Command
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

  test("Writing technology, run Writing, cancel city action") {
    val reg = Helper.ReadAndPlayForTwo("test37/BOARDGAME1.json", "test37/PLAY1.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    val mounted = numof(gg, Civilization.America, CombatUnitType.Mounted)
    println(mounted)
    Helper.executeCommandH(tokenA, "BUYMOUNTED", 2, 2)
    Helper.executeCommandH(tokenC, "WRITINGACTION", -1, -1,""" "Spy" """)
    gg = I.getBoardForToken(tokenA)
    val mounted2 = numof(gg, Civilization.America, CombatUnitType.Mounted)
    println(mounted2)
    assert(mounted == mounted2)

    // find mounted
    val command: Command = gg.play.commands.reverse.find(c => c.command == Command.BUYMOUNTED).get
    println(command.isCanceled)
    assert(command.isCanceled)
  }

  test("Writing technology, send production, do not suspend") {
    val reg = Helper.ReadAndPlayForTwo("test37/BOARDGAME2.json", "test37/PLAY2.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenC)
    println(gg.isSuspended)
    assert(!gg.isSuspended)
    val susp = gg.suspendedForCiv(Civilization.China)
    println(susp)
    //    {"command":"SENDPRODUCTION","civ":"America","p":{"row":2,"col":2},"param":{"row":0,"col":3}}

    Helper.executeCommandH(tokenA, "SENDPRODUCTION", 2, 2, "{\"row\" : 0, \"col\": 3}")
    var gg1: GameBoard = I.getBoardForToken(tokenC)
    println(gg1.isSuspended)
    val susp1 = gg1.suspendedForCiv(Civilization.China)
    println(susp1)
    assert(!gg.isSuspended)
  }

  test("Hanging gardens wonder") {
    val reg = Helper.readBoardAndPlayT("test37/BOARDGAME3.json", "test37/PLAY3.json", Civilization.Egypt)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    assert(! (l contains Command.FREESCOUT))
    assert(! (l contains Command.FREEARMY))
  }

  test("Hanging gardens wonder, next turn") {
    val reg = Helper.readBoardAndPlayT("test37/BOARDGAME3.json", "test37/PLAY4.json", Civilization.Egypt)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val f1 : Seq[MapSquareP] = getFigures(gg, Civilization.Egypt)
    f1.foreach(m => println(m.p))

    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    assert(l contains Command.FREESCOUT)
    assert(l contains Command.FREEARMY)

    // try to get FREESCOUT
    Helper.executeCommandH(token, "FREESCOUT")
    gg = I.getBoardForToken(token)
    val f2 : Seq[MapSquareP] = getFigures(gg, Civilization.Egypt)
    f2.foreach(m => println(m.p))
    assert(f2.length == 3)
    val (a : Int, s : Int) = getNumberOfArmies(gg, Civilization.Egypt)
    println(a + " " + s)
    // two scouts and army
    assert(a == 1 && s == 2)
    // only once
    l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    assert(! (l contains Command.FREESCOUT))
    assert(! (l contains Command.FREEARMY))
  }

}