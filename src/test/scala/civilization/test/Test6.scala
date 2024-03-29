package civilization.test

import civilization.action.{Command, constructCommand}
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.readdir.GenBoard.genBoard
import civilization.message._
import civilization.objects._
import org.scalatest.funsuite.AnyFunSuite
import Helper._



class Test6 extends AnyFunSuite {

  Helper.X

  test("Start game") {
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME1.json", "test6/GAME1.json", Civilization.Germany)
    val trade: Int = numberofTradeH(b, Civilization.Germany).trade
    println(trade)
    var com: Command = constructCommand(Command.RESEARCH, Civilization.Germany, null, toJ("\"Irrigation\""))
    var m: Mess = com.verifyCommand(b)
    println(m)
    assert(m.m == M.CANNOTAFFORDTHISTECHNOLOGY)
    com = constructCommand(Command.RESEARCH, Civilization.Germany, null, toJ("\"HorsebackRiding\""))
    m = playCommand(b, com)
    println(m)
    assert(m == null)
    m = playCommand(b, com)
    println(m)
    assert(m.m == M.CANNOTRESEARCHMORETHENONCEINSINGLETURN)
  }

  test("Start game next research") {
    println("======== next test")
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME1.json", "test6/GAME2.json", Civilization.Germany)
    var com: Command = constructCommand(Command.RESEARCH, Civilization.Germany, null, toJ("\"HorsebackRiding\""))
    var m: Mess = playCommand(b, com)
    println(m)
    assert(m.m == M.TECHNOLOGYRESEARCHEDALREADY)
    com = constructCommand(Command.RESEARCH, Civilization.Germany, null, toJ("\"Pottery\""))
    m = playCommand(b, com)
    println(m)
    assert(m.m == M.CANNOTAFFORDTHISTECHNOLOGY)
  }

  test("Test gen board") {
    println("Test gen")
    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    assert(g != null)
  }

}
