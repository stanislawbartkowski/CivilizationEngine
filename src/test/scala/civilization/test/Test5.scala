package civilization.test

import civilization.action._
import civilization.helper._
import civilization.io.fromjson._
import civilization.message._
import civilization.objects._
import civilization.test.Helper._
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.JsValue


class Test5 extends AnyFunSuite {

  Helper.X

  test("Start game") {
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME1.json", "test5/GAME1.json", Civilization.Germany)
    assert(isCapitalBuild(b, Civilization.Germany))
    assert(citiesForCivilization(b, Civilization.Germany).length == 1)
    //    squaresAround(b,P(2,2)).foreach(println)
    val num: Int = numberofTradeH(b, Civilization.Germany).trade
    println(num)
    assert(num == 6)
    // try to set figure
    var js: String = "{\"row\":2, \"col\" : 2}"
    var j: JsValue = toJ(js)
    var com: Command = constructCommand(Command.SETARMY, Civilization.Germany, P(2, 2), j)
    println(com.verifyCommand(b))
    assert(com.verifyCommand(b) != null)

    js = "{\"row\":2, \"col\" : 3}"
    j = toJ(js)
    com = constructCommand(Command.SETARMY, Civilization.Germany, P(2, 2), j)
    println(com.verifyCommand(b))
    assert(com.verifyCommand(b) != null)

    js = "{\"row\":1, \"col\" : 3}"
    j = toJ(js)
    com = constructCommand(Command.SETARMY, Civilization.Germany, P(2, 2), j)
    assert(com.verifyCommand(b) == null)
    com.executeCommand(b)
    val m: MapSquareP = getSquare(b, P(1, 3))
    println(m)
    println(m.s.figures)
    assert(m.s.figures.civ == Civilization.Germany)
    assert(m.s.figures.numberofArmies == 1)
    assert(m.s.figures.numberofScouts == 0)
  }

  test("Start game 1") {
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME1.json", "test5/GAME2.json", Civilization.Germany)
    val prod: Int = getProductionForCityH(b, Civilization.Germany, P(2, 2)).prod
    println(prod)
    assert(prod == 5)
    val figures: Seq[MapSquareP] = getFigures(b, Civilization.Germany)
    figures.foreach(println)
    assert(figures.length == 2)
    var count: (Int, Int) = getNumberOfArmies(b, Civilization.Germany)
    println(count)
    println("=======================")
    assert(count._1 == 1)
    assert(count._2 == 1)
    var js: String = "{\"row\":1, \"col\" : 1}"
    var j: JsValue = toJ(js)
    var com1: Command = constructCommand(Command.BUYARMY, Civilization.Germany, P(2, 2), j)
    println(com1.verifyCommand(b))
    assert(com1.verifyCommand(b) == null)
    js = "{\"row\":1, \"col\" : 1}"
    j = toJ(js)
    // cannot afford figure
    var com2: Command = constructCommand(Command.BUYSCOUT, Civilization.Germany, P(2, 2), j)
    println(com2.verifyCommand(b))
    assert(com2.verifyCommand(b) != null)
    com1.executeCommand(b)
    val m: MapSquareP = getSquare(b, P(1, 1))
    println(m)
    assert(m.s.figures.numberofArmies == 1)
    assert(m.s.figures.numberofScouts == 0)
    assert(m.s.figures.civ == Civilization.Germany)
    count = getNumberOfArmies(b, Civilization.Germany)
    println(count)
    assert(count._1 == 2)
    assert(count._2 == 1)
  }

  test("Start game 2") {
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME1.json", "test5/GAME3.json", Civilization.Germany)
    var js: String = "{\"row\":1, \"col\" : 2}"
    var j: JsValue = toJ(js)
    var com1: Command = constructCommand(Command.BUYARMY, Civilization.Germany, P(2, 2), j)
    var m: Mess = playCommand(b, com1)
    println(m)
    assert(m != null)
    var com2: Command = constructCommand(Command.ENDOFPHASE, Civilization.Germany, null, toJ("\"CityManagement\""))
    m = playCommand(b, com2)
    assert(m == null)
    println("===========================")
    com2 = constructCommand(Command.STARTMOVE, Civilization.Germany, P(0, 0), toJ("{\"numberofArmies\" : 1}"))
    m = playCommand(b, com2)
    println(m)
    assert(m != null)
    println("===========================")
    val ma: MapSquareP = getSquare(b, P(1, 1))
    println(ma)
    com2 = constructCommand(Command.STARTMOVE, Civilization.Germany, P(1, 1), toJ("{\"numberofArmies\" : 2}"))
    m = playCommand(b, com2)
    println(m)
    assert(m != null)
    println("===========================")
    com2 = constructCommand(Command.STARTMOVE, Civilization.Germany, P(1, 1), toJ("{\"numberofArmies\" : 1, \"numberofScouts\" : 1}"))
    m = playCommand(b, com2)
    println(m)
    assert(m != null)
    println("===========================")
    com2 = constructCommand(Command.STARTMOVE, Civilization.Germany, P(1, 1), toJ("{\"numberofArmies\" : 1}"))
    m = playCommand(b, com2)
    println(m)
    assert(m == null)
    println("try again")
    com2 = constructCommand(Command.STARTMOVE, Civilization.Germany, P(1, 1), toJ("{\"numberofArmies\" : 1}"))
    m = playCommand(b, com2)
    println(m)
    assert(m != null)
  }

  test("Start game 4") {
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME1.json", "test5/GAME4.json", Civilization.Germany)
    println("Test no 4 =========================")
    var com2: Command = constructCommand(Command.STARTMOVE, Civilization.Germany, P(1, 1), toJ("{\"numberofArmies\" : 1, \"numberofScouts\" : 0}"))
    var m: Mess = playCommand(b, com2)
    println(m)
    assert(m != null)
    println("============")
    com2 = constructCommand(Command.MOVE, Civilization.Germany, P(3, 1))
    m = playCommand(b, com2)
    println(m)
    assert(m != null)
    println("============")
    com2 = constructCommand(Command.MOVE, Civilization.Germany, P(0, 3))
    m = playCommand(b, com2)
    println(m)
    assert(m != null)
    println("============")
    com2 = constructCommand(Command.MOVE, Civilization.Germany, P(2, 2))
    m = playCommand(b, com2)
    println(m)
    assert(m != null)
    println("============")
    com2 = constructCommand(Command.MOVE, Civilization.Germany, P(0, 1))
    m = playCommand(b, com2)
    println(m)
    assert(m == null)
    var ms: MapSquareP = getSquare(b, P(1, 1))
    println(ms)
    assert(ms.s.figures.civ == null)
    ms = getSquare(b, P(0, 1))
    println(ms)
    assert(ms.s.figures.civ == Civilization.Germany)
    assert(ms.s.figures.numberofArmies == 1)
    println("============")
    com2 = constructCommand(Command.MOVE, Civilization.Germany, P(0, 0))
    m = playCommand(b, com2)
    println(m)
    println("============")
    com2 = constructCommand(Command.STARTMOVE, Civilization.Germany, P(3, 1), toJ("{\"numberofArmies\" : 1, \"numberofScouts\" : 0}"))
    m = playCommand(b, com2)
    println(m)
    assert(m.m == M.LASTMOVENOTENDED)
    var com3: Command = constructCommand(Command.ENDOFMOVE, Civilization.Germany, null)
    m = playCommand(b, com3)
    println(m)
    assert(m == null)
    // execute again
    m = playCommand(b, com2)
    println(m)
    assert(m == null)
    println("move to hidden")
    com2 = constructCommand(Command.MOVE, Civilization.Germany, P(4, 1))
    m = playCommand(b, com2)
    println(m)
    assert(m.m == M.POINTNOTREVEALED)
  }

  test("Start game 5") {
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME1.json", "test5/GAME5.json", Civilization.Germany)
    println("Test no 5 =========================")
    var com: Command = constructCommand(Command.REVEALTILE, Civilization.Germany, P(1, 0), toJ("\"Left\""))
    var m: Mess = playCommand(b, com)
    println(m)
    assert(m.m == M.CANNOTREVEALFROMTHISPOINT)
    com = constructCommand(Command.REVEALTILE, Civilization.Germany, P(1, 0), toJ("\"Down\""))
    m = playCommand(b, com)
    println(m)
    assert(m == null)
    com = constructCommand(Command.ENDOFMOVE, Civilization.Germany, P(4, 1))
    m = playCommand(b, com)
    println(m)
    assert(m.m == M.CANNOTCROSSWATER)
    com = constructCommand(Command.ENDOFMOVE, Civilization.Germany, P(2, 1))
    m = playCommand(b, com)
    println(m)
    assert(m == null)

    com = constructCommand(Command.STARTMOVE, Civilization.Germany, P(2, 1), toJ("{\"numberofScouts\" : 1}"))
    m = playCommand(b, com)
    println(m)
    assert(m == null)
    com = constructCommand(Command.MOVE, Civilization.Germany, P(3, 1))
    m = playCommand(b, com)
    println(m)
    assert(m == null)
    var count: (Int, Int) = getNumberOfArmies(b, Civilization.Germany)
    println(count)
    assert(count._1 == 2)
    assert(count._2 == 1)
    com = constructCommand(Command.MOVE, Civilization.Germany, P(4, 1))
  }

  test("Start game 6") {
    var (token,b) = Helper.readBoardAndPlayT("test5/BOARDGAME2.json", "test5/GAME6.json", Civilization.Germany)
    println("Test no 6 =========================")
    var com: Command = constructCommand(Command.STARTMOVE, Civilization.Germany, P(3, 1), toJ("{\"numberofArmies\" : 1}"))
    var m: Mess = playCommand(b, com)
    println(m)
    assert(m == null)
    com = constructCommand(Command.MOVE, Civilization.Germany, P(2, 1))
    m = playCommand(b, com)
    println(m)
    assert(m == null)
    com = constructCommand(Command.ENDOFMOVE, Civilization.Germany, P(2, 1))
    m = playCommand(b, com)
    println(m)
    assert(m == null)
    com = constructCommand(Command.STARTMOVE, Civilization.Germany, P(4, 2), toJ("{\"numberofScouts\" : 1}"))
    m = playCommand(b, com)
    println(m)
    assert(m != null)
//    assert(m.m == M.FIGURESMOVEDAGAIN)
    assert(m.m == M.NUMBEROFSCOUTSLESS)
  }
}