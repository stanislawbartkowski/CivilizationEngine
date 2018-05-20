package civilization.test

import civilization.I
import civilization.I.registerGame
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper.getProductionForCity
import civilization.objects.{Civilization, Command, P}
import org.scalatest.FunSuite
import civilization.io.fromjson._
import play.api.libs.json.JsArray
import Helper.II


class Test13 extends FunSuite {

  Helper.I

  test("Test no scouts") {
    val bb: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test13/GAME1.json", Civilization.Rome)
    val token: String = registerGame(bb, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
  }

  test("Test, scout but no production, outskirts") {
    val bb: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test13/GAME2.json", Civilization.Rome)
    val token: String = registerGame(bb, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    // no production outskirts
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
  }

  test("Test, scout production") {
    val bb: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test13/GAME3.json", Civilization.Rome)
    val token: String = registerGame(bb, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isDefined)
    var prodc = getProductionForCity(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(6 == prodc.prod)
    Helper.executeCommandH(token, "SENDPRODUCTION", 2, 2, "{\"row\" : 3, \"col\": 0}")
    g = I.getBoardForToken(token)
    l = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
    assert(l.find(_ == Command.UNDOSENDPRODUCTION).isDefined)
    prodc = getProductionForCity(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(7 == prodc.prod)
    assert(1 == prodc.fromscouts)
    // now undo
    Helper.executeCommandH(token, "UNDOSENDPRODUCTION", 2, 2, "{\"row\" : 3, \"col\": 0}")
    g = I.getBoardForToken(token)
    l = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isDefined)
    prodc = getProductionForCity(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(6 == prodc.prod)
    assert(0 == prodc.fromscouts)
    // itemized
    var s: String = II.itemizeCommand(token, "SENDPRODUCTION")
    assert(s != null)
    println(s)
  }

  test("Test, scout on not productive square") {
    val bb: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test13/GAME4.json", Civilization.Rome)
    val token: String = registerGame(bb, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
  }

  test("Test, two scouts") {
    val bb: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test13/GAME5.json", Civilization.Rome)
    val token: String = registerGame(bb, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
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
    l = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.UNDOSENDPRODUCTION).isDefined)
    assert(l.find(_ == Command.SENDPRODUCTION).isEmpty)
    var prodc = getProductionForCity(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(8 == prodc.prod)
    assert(2 == prodc.fromscouts)
    Helper.executeCommandH(token, "UNDOSENDPRODUCTION", 2, 2, "{\"row\" : 3, \"col\": 0}")
    g = I.getBoardForToken(token)
    l = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.UNDOSENDPRODUCTION).isDefined)
    assert(l.find(_ == Command.SENDPRODUCTION).isDefined)
    prodc = getProductionForCity(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(7 == prodc.prod)
    assert(1 == prodc.fromscouts)
  }


}
