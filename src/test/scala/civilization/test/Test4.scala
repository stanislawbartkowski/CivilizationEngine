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
    println(com.verify(b))
    assert(com.verify(b) != null)
    revealTile(b, Orientation.Down, P(0, 0))
    assert(com.verify(b) == null)
    // build capital
    com.execute(b)
    println(com.verify(b))
    assert(com.verify(b) != null)
    // reveal next
    revealTile(b, Orientation.Down, P(1, 0))
    val com1: Command = constructCommand(Command.SETCITY, Civilization.Germany, P(4, 1), null)
    println(com1.verify(b))
    assert(com1.verify(b) != null)
    val com2: Command = constructCommand(Command.SETCITY, Civilization.Germany, P(4, 2), null)
    println(com2.verify(b))
    assert(com2.verify(b) != null)

    val coms: Command = constructCommand(Command.FORCEDMOVEFIGURES, Civilization.Germany, P(5, 2), toJ("{ \"numberofArmies\" : 0, \"numberofScouts\" : 1}"))
    var m: Mess = playCommand(b, coms)
    println(m)

    val com3: Command = constructCommand(Command.SETCITY, Civilization.Germany, P(5, 2), null)
    println(com3.verify(b))
    assert(com3.verify(b) == null)
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
