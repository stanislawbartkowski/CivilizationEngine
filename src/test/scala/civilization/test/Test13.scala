package civilization.test

import civilization.I
import civilization.gameboard.GameBoard
import civilization.helper.getProductionForCity
import civilization.objects.{Civilization, Command, P}
import org.scalatest.funsuite.AnyFunSuite
import civilization.io.fromjson._
import play.api.libs.json.JsArray
import Helper._


class Test13 extends AnyFunSuite {

  Helper.X

  test("Test no scouts") {
    var (token,bb) = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test13/GAME1.json", Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
  }

  test("Test, scout but no production, outskirts") {
    var (token,b) = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test13/GAME2.json", Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    // no production outskirts
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
  }

  test("Test, scout production") {
    var (token,b) = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test13/GAME3.json", Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isDefined)
    var prodc = getProductionForCityH(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(6 == prodc.prod)
    Helper.executeCommandH(token, "SENDPRODUCTION", 2, 2, "{\"row\" : 3, \"col\": 0}")
    g = I.getBoardForToken(token)
    l = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
    assert(l.find(_ == Command.UNDOSENDPRODUCTION).isDefined)
    prodc = getProductionForCityH(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(7 == prodc.prod)
    assert(1 == prodc.fromscouts)
    // now undo
    Helper.executeCommandH(token, "UNDOSENDPRODUCTION", 2, 2, "{\"row\" : 3, \"col\": 0}")
    g = I.getBoardForToken(token)
    l = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isDefined)
    prodc = getProductionForCityH(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(6 == prodc.prod)
    assert(0 == prodc.fromscouts)
    // itemized
    var s: String = II.itemizeCommand(token, "SENDPRODUCTION")
    assert(s != null)
    println(s)
  }

  test("Test, scout on not productive square") {
    var (token,b) = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test13/GAME4.json", Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
  }

  test("Test, two scouts") {
    var (token,b) = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test13/GAME5.json", Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    var s: String = II.itemizeCommand(token, "SENDPRODUCTION")
    assert(s != null)
    println(s)
    val a : JsArray = toJ(s).as[JsArray]
    assert(2 == a.value.size)
    println(a)
    Helper.executeCommandH(token, "SENDPRODUCTION", 2, 2, "{\"row\" : 3, \"col\": 0}")
    Helper.executeCommandH(token, "SENDPRODUCTION", 2, 2, "{\"row\" : 0, \"col\": 2}")
    g = I.getBoardForToken(token)
    l = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.UNDOSENDPRODUCTION).isDefined)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
    var prodc = getProductionForCityH(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(8 == prodc.prod)
    assert(2 == prodc.fromscouts)
    Helper.executeCommandH(token, "UNDOSENDPRODUCTION", 2, 2, "{\"row\" : 3, \"col\": 0}")
    g = I.getBoardForToken(token)
    l = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.UNDOSENDPRODUCTION).isDefined)
    assert(l.find(_ == Command.SENDPRODUCTION).isDefined)
    prodc = getProductionForCityH(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(7 == prodc.prod)
    assert(1 == prodc.fromscouts)
  }


}
