package civilization.test

import civilization.I.{CurrentGame, II, executeCommand}
import civilization.{I, RR}

import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson.toJ
import org.scalatest.FunSuite
import civilization.objects._
import play.api.libs.json._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GenBoard.genBoard

class Test20 extends FunSuite with ImplicitMiximFromJson {

  Helper.I

  test("Two players game") {
    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    println(token)
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = II.joinGame(gameid, Civilization.China.toString)
    // Rome active at the beginning
    Helper.activeciv(token, "Rome", "StartOfTurn")
    // Rome also active for China
    Helper.activeciv(ctoken, "Rome", "StartOfTurn")
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // now China active
    Helper.activeciv(token, "China", "StartOfTurn")
    // Rome also active for China
    Helper.activeciv(ctoken, "China", "StartOfTurn")
    // goto research
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // now trade, both are active
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Trade\"")
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"Trade\"")
    // Rome active
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now movement
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"Movement\"")
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Movement\"")
    // reserach: both are active
    // now China active
    Helper.activeciv(token, "Rome", "Research")
    // Rome also active for China
    Helper.activeciv(ctoken, "China", "Research")

    // next turn, who is first, expected China (not Rome)
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"Research\"")
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Research\"")
    Helper.activeciv(token, "China", "StartOfTurn")
    // Rome also active for China
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // now Rome
    Helper.activeciv(token, "Rome", "StartOfTurn")
    Helper.activeciv(ctoken, "Rome", "StartOfTurn")
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // trade
    // now trade, both are active
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Trade\"")
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"Trade\"")
    // now city management
    // again, China first
    // now China active
    Helper.activeciv(token, "China", "CityManagement")
    // China
    Helper.activeciv(ctoken, "China", "CityManagement")
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now Rome active
    Helper.activeciv(token, "Rome", "CityManagement")
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now movement
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"Movement\"")
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Movement\"")
    // reserach: both are active
    // now China active
    Helper.activeciv(token, "Rome", "Research")
    // Rome also active for China
    Helper.activeciv(ctoken, "China", "Research")
    // next turn : now Rome first
    executeCommand(token, "ENDOFPHASE", -1, -1, "\"Research\"")
    executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Research\"")
    // Rome again in turn
    Helper.activeciv(token, "Rome", "StartOfTurn")
    // Rome also active for China
    Helper.activeciv(ctoken, "Rome", "StartOfTurn")
  }

  test("Two players game, build capital on the other player home tile") {
    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    println(token)
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = II.joinGame(gameid, Civilization.China.toString)
    val g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    val i: String = II.itemizeCommand(token, "SETCAPITAL")
    val j: JsValue = toJ(i)
    val a: JsArray = j.as[JsArray]
    println(j)
    a.value.foreach(e => {
      println(e)
      val p : P = e
      val s : MapSquareP = getSquare(g,p)
      println(s.t.tile.civhome)
      println(s.t.tile.civ)
      assert(s.t.tile.civhome)
      assert(s.t.tile.civ == Civilization.Rome)
    })
  }

  test("Two players game, building in turn") {
    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    println(token)
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = II.joinGame(gameid, Civilization.China.toString)
    val g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    // allowed commands for Rome
    println(l)
    assert(l.find(_ == Command.SETCAPITAL).isDefined)
    l = allowedCommands(g, Civilization.China)
    // allowed commands for Rome
    println(l)
    assert(l.isEmpty)
    // test list of civs
  }

  test("Two players game, battle, army into water") {
    val reg = Helper.ReadAndPlayForTwo("test20/BOARDGAME1.json", "test20/PLAY1.json", Civilization.Spain, Civilization.Arabs)
    val tokens: String = reg._1
    val tokena: String = reg._2
    Helper.executeCommandH(tokens, "SETCITY", 2, 5)
    val g: GameBoard = I.getBoardForToken(tokens)
    val fig : Seq[MapSquareP] = getFigures(g, Civilization.Spain)
    fig.foreach(s => {
      println(s)
      assert(s.sm.terrain != Terrain.Water)
    })
  }

  test("Test gen board and civs") {
    println("Test gen")
    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    assert(g != null)
    assert(!g.civil.isEmpty)
    g.civil.foreach(println)
    val s : String = II.getData(II.LISTOFCIVDESCR)
    println(s)
    g.tech.foreach(println)
    // check that CodeOfLaw enables Republic
    assert(g.tech.find(_.tech == TechnologyName.CodeOfLaw).get.gover.get == GovernmentName.Republic)
  }


}
