package civilization.test

import civilization.I.{CurrentGame}
import civilization.{I, RR}
import civilization.gameboard.GameBoard
import civilization.gameboard._
import civilization.helper._
import civilization.io.fromjson.toJ
import org.scalatest.funsuite.AnyFunSuite
import civilization.objects._
import play.api.libs.json._
import civilization.io.readdir._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GenBoard.genBoard
import civilization.io.readdir.GameResources
import Helper._


class Test20 extends AnyFunSuite with ImplicitMiximFromJson {

  Helper.X

  test("Two players game") {
//    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    val token : String = registerOwnerTwo("Rome,China")
    println(token)
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val (ctoken: String, gameId : Int) = civilization.I.decodeS(II.joinGame(gameid, Civilization.China.toString))
    // Rome active at the beginning
    Helper.activeciv(token, "Rome", "StartOfTurn")
    // Rome also active for China
    Helper.activeciv(ctoken, "Rome", "StartOfTurn")
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // now China active
    Helper.activeciv(token, "China", "StartOfTurn")
    // Rome also active for China
    Helper.activeciv(ctoken, "China", "StartOfTurn")
    // goto research
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // now trade, both are active
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"Trade\"")
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"Trade\"")
    // Rome active
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now movement
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"Movement\"")
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"Movement\"")
    // reserach: both are active
    // now China active
    Helper.activeciv(token, "Rome", "Research")
    // Rome also active for China
    Helper.activeciv(ctoken, "China", "Research")

    // next turn, who is first, expected China (not Rome)
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"Research\"")
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"Research\"")
    Helper.activeciv(token, "China", "StartOfTurn")
    // Rome also active for China
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // now Rome
    Helper.activeciv(token, "Rome", "StartOfTurn")
    Helper.activeciv(ctoken, "Rome", "StartOfTurn")
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    // trade
    // now trade, both are active
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"Trade\"")
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"Trade\"")
    // now city management
    // again, China first
    // now China active
    Helper.activeciv(token, "China", "CityManagement")
    // China
    Helper.activeciv(ctoken, "China", "CityManagement")
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now Rome active
    Helper.activeciv(token, "Rome", "CityManagement")
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now movement
    Helper.activeciv(token, "China", "Movement")
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"Movement\"")
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"Movement\"")
    // reserach: both are active
    // now China active
    Helper.activeciv(token, "Rome", "Research")
    // Rome also active for China
    Helper.activeciv(ctoken, "China", "Research")
    // next turn : now Rome first
    executeCommandH(token, "ENDOFPHASE", -1, -1, "\"Research\"")
    executeCommandH(ctoken, "ENDOFPHASE", -1, -1, "\"Research\"")
    // Rome again in turn
    Helper.activeciv(token, "Rome", "StartOfTurn")
    // Rome also active for China
    Helper.activeciv(ctoken, "Rome", "StartOfTurn")
  }

  test("Two players game, build capital on the other player home tile") {
//    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    val token : String = registerOwnerTwo("Rome,China")
    println(token)
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = II.joinGame(gameid, Civilization.China.toString)
    val g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    println(l)
    val i: String = II.itemizeCommand(token, "SETCAPITAL")
    val j: JsValue = toJ(i)
    val a: JsArray = j.as[JsArray]
    println(j)
    a.value.foreach(e => {
      println(e)
      val p: P = e
      val s: MapSquareP = getSquare(g, p)
      println(s.t.tile.civhome)
      println(s.t.tile.civ)
      assert(s.t.tile.civhome)
      assert(s.t.tile.civ.get == Civilization.Rome)
    })
  }

  test("Two players game, building in turn") {
//    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    val token : String = registerOwnerTwo("Rome,China")
    println(token)
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = II.joinGame(gameid, Civilization.China.toString)
    val g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Rome)
    // allowed commands for Rome
    println(l)
    assert(l.find(_ == Command.SETCAPITAL).isDefined)
    l = allowedCommandsH(g, Civilization.China)
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
    val fig: Seq[MapSquareP] = getFigures(g, Civilization.Spain)
    fig.foreach(s => {
      println(s)
      assert(s.sm.terrain != Terrain.Water)
    })
  }

  test("Test gen board and civs") {
    println("Test gen")
    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    assert(g != null)
    val gr: GameResources = GameResources.instance()
    assert(!gr.civ.isEmpty)
    gr.civ.foreach(println)
    val s: String = II.getData(II.LISTOFRES)
    println(s)
    gr.tech.foreach(println)
    // check that CodeOfLaw enables Republic
    assert(gr.tech.find(_.tech == TechnologyName.CodeOfLaw).get.gover.get == GovernmentName.Republic)
  }

  test("Two players game, check JSon for technologies") {
//    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    val token : String = registerOwnerTwo("Rome,China")
    println(token)
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = II.joinGame(gameid, Civilization.China.toString)
    val s: String = II.getData(II.GETBOARDGAME, token)
    val j: JsValue = toJ(s)
    println(s)
    //    val tecj: JsArray = (j \ "board" \ "tech").as[JsArray]
    //    println(tecj)
    val y: JsArray = (j \ "board" \ "you" \ "tech").as[JsArray]
    println(y)
    assert(1 == y.value.length)
    val tech: JsValue = y.value(0)
    println(tech)
    assert("CodeOfLaw" == (tech \ "tech").as[String])
    assert(1 == (tech \ "level").as[Int])
    val yy: JsValue = (j \ "board" \ "you").as[JsValue]
    println(yy)
    assert("Republic" == (yy \ "gover").as[String])
    val jb = II.getData(II.LISTOFRES)
    val jj: JsValue = toJ(jb)
    val tecj: JsArray = (jj \ "tech").as[JsArray]
    println(tecj)
  }

  test("Test research") {
    val reg = Helper.readBoardAndPlayT("test11/BOARDGAME1.json", "test20/GAME2.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"CityManagement"""")
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """"Movement"""")
    g = I.getBoardForToken(token)
    var l = allowedCommandsH(g, Civilization.Rome)
    // allowed commands for Rome
    println(l)
    // research enabled
    assert(l.find(_ == Command.RESEARCH).isDefined)
  }

  test("Technology, block second level if not enough place") {
    val reg = Helper.readBoardAndPlayT("test20/BOARDGAME2.json", "test20/PLAY2.json", Civilization.America)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.America)
    println(l)
    assert(l.find(_ == Command.RESEARCH).isDefined)
    val t = techologyLevel(g, g.playerDeck(Civilization.America))
    println(t)
    // level 1 although trade 12, not place for 2 level technology
    assert(t == Set(1))
    val trade = numberofTradeH(g, Civilization.America)
    println(trade.trade)
    assert(trade.trade == 12)
  }

  test("Technology, after research end of turn") {
    val reg = Helper.readBoardAndPlayT("test20/BOARDGAME2.json", "test20/PLAY2.json", Civilization.America)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.America)
    println(l)
    assert(l.find(_ == Command.RESEARCH).isDefined)
    //    com = constructCommand(Command.RESEARCH, Civilization.Germany, null, toJ("\"HorsebackRiding\""))
    Helper.executeCommandH(token, "RESEARCH", -1, -1, "\"HorsebackRiding\"")
    g = I.getBoardForToken(token)
    // again commands
    l = allowedCommandsH(g, Civilization.America)
    println(l)
    assert(l.find(_ == Command.RESEARCH).isEmpty)
  }

  test("Research, next test") {
    val reg = Helper.ReadAndPlayForTwo("test20/BOARDGAME3.json", "test20/PLAY3.json", Civilization.Arabs, Civilization.Egypt)
    val tokena: String = reg._1
    val tokene: String = reg._2
    var g: GameBoard = I.getBoardForToken(tokena)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.Arabs)
    println(l)
    assert(l.find(_ == Command.RESEARCH).isEmpty)
  }

  test("Research, next test, after end of research") {
    val reg = Helper.ReadAndPlayForTwo("test20/BOARDGAME4.json", "test20/PLAY4.json", Civilization.America, Civilization.Russia)
    val tokena: String = reg._1
    val tokenr: String = reg._2
    var g: GameBoard = I.getBoardForToken(tokena)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.America)
    // endofresearch issued for America
    println(l)
    assert(l.find(_ == Command.RESEARCH).isEmpty)
    l = allowedCommandsH(g, Civilization.Russia)
    // end of research issued for America
    println(l)
    assert(l.find(_ == Command.RESEARCH).isDefined)
    assert(l.find(_ == Command.ENDOFPHASE).isDefined)
  }

  test("Test wonders") {
    println("List of wonders")
    val wonders: Seq[WondersOfTheWorld] = readListOfWonders
    val wo: WondersOfTheWorld = wonders.find(_.name == Wonders.ChichenItza).get
    assert(wo.cost == 15)
    assert(wo.age == WondersAge.Ancient)
    assert(wo.phase.get == TurnPhase.CityManagement)
    assert(wo.discount.get == WondersDiscount(10, TechnologyName.Currency))
  }

  test("Test technologies") {
    println("List of  technologies")
    val l: Seq[Technology] = readTechnologies
    println(l)
  }

  test("Test buildings") {
    println("List of buildings")
    val l: Seq[Building] = readListOfBuildings
    println(l)
    var b: Building = l.find(_.name == BuildingName.University).get
    println(b)
    assert(b.cost == 8)
    assert(b.tokens.numofCulture == 2)
    b = l.find(_.name == BuildingName.Barracks).get
    println(b)
    assert(b.tokens.numofBattle == 2)
    assert(b.upgrade.get == BuildingName.Academy)
    assert(b.star.get)
    b = l.find(_.name == BuildingName.Bank).get
    println(b)
    assert(b.tokens.numofCoins == 1)
  }

  test("Gen BoardGJ, check buildings") {

    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    println(g.market.buildings.table)
    assert(g.market.buildings.table.size > 0)
    g.market.buildings.table.foreach(t => assert(t._2 >= 5))
    val (token: String, gameid : Int) = civilization.I.decodeS(civilization.I.registerGame(g, Civilization.Germany))
    var ss = II.getData(II.GETBOARDGAME, token)
    println(ss)
  }

}
