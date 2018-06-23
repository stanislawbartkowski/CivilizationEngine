package civilization.test

import civilization.action._
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson._
import civilization.message.Mess
import civilization.objects._
import org.scalatest.FunSuite
import Helper.II


class Test4 extends FunSuite {

  Helper.I

  test("Check city") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME1.json")
    revealTile(b, Orientation.Down, P(0, 0))
    assert(!isCapitalBuild(b, Civilization.Germany))
    assert(isSquareForCity(b, P(2, 2),Civilization.Germany).isEmpty)
    assert(isSquareForCity(b, P(2, 3),Civilization.Germany).isDefined)
    assert(isSquareForCity(b, P(0, 0),Civilization.Germany).isDefined)
    assert(isSquareForCity(b, P(3, 1),Civilization.Germany).isDefined)
    assert(isSquareForCity(b, P(2, 1),Civilization.Germany).isEmpty)
  }

  test("Check command") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME1.json")
    val com: Command = constructCommand(Command.SETCAPITAL, Civilization.Germany, P(2, 2), null)
    println(com.verifyCommand(b))
    assert(com.verifyCommand(b) != null)
    revealTile(b, Orientation.Down, P(0, 0))
    assert(com.verifyCommand(b) == null)
    // build capital
    com.executeCommand(b)
    println(com.verifyCommand(b))
    assert(com.verifyCommand(b) != null)
    // reveal next
    revealTile(b, Orientation.Down, P(1, 0))
    val com1: Command = constructCommand(Command.SETCITY, Civilization.Germany, P(4, 1), null)
    println(com1.verifyCommand(b))
    assert(com1.verifyCommand(b) != null)
    val com2: Command = constructCommand(Command.SETCITY, Civilization.Germany, P(4, 2), null)
    println(com2.verifyCommand(b))
    assert(com2.verifyCommand(b) != null)

    val coms: Command = constructCommand(Command.FORCEDMOVEFIGURES, Civilization.Germany, P(5, 2), toJ("{ \"numberofArmies\" : 0, \"numberofScouts\" : 1}"))
    var m: Mess = playCommand(b, coms)
    println(m)

    val com3: Command = constructCommand(Command.SETCITY, Civilization.Germany, P(5, 2), null)
    println(com3.verifyCommand(b))
    assert(com3.verifyCommand(b) == null)
    println("==================")
    val ms: MapSquareP = getSquare(b, P(5, 2))
    println(ms)

    m = playCommand(b, com3)
    println(m)
    //    assert (m != null)
    //    com3.execute(b)
    assert(isCapitalBuild(b, Civilization.Germany))
    assert(citiesForCivilization(b, Civilization.Germany).length == 2)
  }

}
