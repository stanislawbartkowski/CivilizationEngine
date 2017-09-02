package civilization.test

import civilization.action.{Command, constructCommand}
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.objects._
import civilization.io.fromjson._
import civilization.message._
import org.scalatest.FunSuite
import civilization.io.readdir._
import civilization.io.readdir.GenBoard.genBoard


class Test6 extends FunSuite {

  Helper.I

  test("Start game") {
    val b: GameBoard = Helper.readBoardAndPlay("test5/BOARDGAME1.json", "test6/GAME1.json", Civilization.Germany)
    val trade: Int = numberofTrade(b, Civilization.Germany)
    println(trade)
    var com: Command = constructCommand(Command.RESEARCH, Civilization.Germany, null, toJ("\"Irigation\""))
    var m: Mess = com.verify(b)
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
    val b: GameBoard = Helper.readBoardAndPlay("test5/BOARDGAME1.json", "test6/GAME2.json", Civilization.Germany)
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
