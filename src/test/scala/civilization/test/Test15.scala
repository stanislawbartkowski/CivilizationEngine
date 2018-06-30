package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson.toJ
import civilization.message._
import civilization.objects._
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue}
import Helper._

class Test15 extends FunSuite {

  Helper.I

  test("Test buy units") {
    val reg = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test13/GAME2.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.BUYARTILLERY).isDefined)
    assert(l.find(_ == Command.BUYMOUNTED).isDefined)
    assert(l.find(_ == Command.BUYINFANTRY).isDefined)
    assert(l.find(_ == Command.BUYAIRCRAFT).isEmpty)
    Helper.executeCommandFail(token, "BUYARTILLERY", 1, 1, null)
    Helper.executeCommandFail(token, "BUYAIRCRAFT", 2, 2, null)
    Helper.executeCommandH(token, "BUYARTILLERY", 2, 2, null)
    g = I.getBoardForToken(token)
    assert(1 == g.playerDeck(Civilization.Rome).units.length)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"CityManagement"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"Movement"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"Research"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"StartOfTurn"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"Trade"""")
    val thrown = intercept[civilization.message.FatalError] {
      // all units used
      Helper.executeCommandFail(token, "BUYARTILLERY", 2, 2, null)
    }
    assert(M.ALLUNITSUSED == thrown.m.m)
  }

  private def runtest(): String = {
    val reg = Helper.readBoardAndPlayT("test15/BOARDGAME1.json", "test15/GAME1.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    assert(g.playerDeck(Civilization.Rome).units.length == 1)
    assert(!g.market.killedunits.isEmpty)
    // should reuse killed
    Helper.executeCommandH(token, "BUYARTILLERY", 2, 2, null)
    g = I.getBoardForToken(token)
    assert(g.market.killedunits.isEmpty)
    assert(2 == g.playerDeck(Civilization.Rome).units.length)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"CityManagement"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"Movement"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"Research"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"StartOfTurn"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"Trade"""")
    val thrown = intercept[civilization.message.FatalError] {
      // all units used
      Helper.executeCommandFail(token, "BUYARTILLERY", 2, 2, null)
    }
    assert(M.ALLUNITSUSED == thrown.m.m)
    token
  }

  test("Test buy units, resuse kill units") {
    runtest
  }

  test("Test buy units, genboard") {
    val token: String = runtest
    val b: String = II.getData(II.GETBOARDGAME, token)
    println(b)
    val j = toJ(b)
    val you: JsValue = (j \ "board" \ "you").as[JsValue]
    println(you)
    //    val mili : Int = (you \ "militarytech").as[Int]
    //    assert(0 == mili)
    var uni: JsArray = (you \ "units" \ "units").as[JsArray]
    println(uni)
    uni = (you \ "units" \ "list").as[JsArray]
    println(uni)
  }

  test("Test round no") {
    val reg = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test13/GAME2.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    val pha: CurrentPhase = currentPhase(g)
    println(pha)
    assert(0 == pha.roundno)
  }

  test("Test round no with start game") {
    val token: String = II.getData(II.REGISTEROWNER, "China")
    println(token)
    val s = II.getData(II.GETBOARDGAME, token)
    Helper.executeCommandH(token, "SETCAPITAL", 2, 2, null)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"StartOfTurn"""")
    var g: GameBoard = I.getBoardForToken(token)
    val pha: CurrentPhase = currentPhase(g)
    println(pha)
    assert(0 == pha.roundno)
    assert(TurnPhase.Trade == pha.turnPhase)
    val bs: String = II.getData(II.GETBOARDGAME, token)
    println(bs)
  }

  test("Test round no get from game") {
    val reg = Helper.readBoardAndPlayT("test15/BOARDTEST.json", "test15/GAME2.json", Civilization.America)
    val token = reg._1
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"StartOfTurn"""")
    var g: GameBoard = I.getBoardForToken(token)
    val pha: CurrentPhase = currentPhase(g)
    println(pha)
    assert(0 == pha.roundno)
    assert(TurnPhase.Trade == pha.turnPhase)
  }

  test("Test buyunit end of city action") {
    val reg = Helper.readBoardAndPlayT("test15/BOARDGAME1.json", "test15/GAME1.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    // should reuse killed
    Helper.executeCommandH(token, "BUYARTILLERY", 2, 2, null)
    g = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.BUYARTILLERY).isEmpty)
    assert(l.find(_ == Command.SPENDTRADE).isEmpty)
  }


  }
