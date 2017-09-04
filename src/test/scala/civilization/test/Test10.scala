package civilization.test

import civilization.I.{CurrentGame, WaitingGames, executeCommand}
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.objects._
import civilization.{I, II, RR}
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import play.api.libs.json.JsValue
import civilization.io.fromjson.toJ


class Test10 extends FunSuite {

  Helper.I

  test("Register and unregister") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME1.json", Civilization.Rome)
    val token: String = cu._1
    val game: String = II.getData(II.GETBOARDGAME, token)
    println(game)
    //  unregister
    II.getData(II.UNREGISTERTOKEN, token)
    // should throw exception
    an[Exception] should be thrownBy II.getData(II.GETBOARDGAME, token)
  }


  test("Read two player game") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME1.json", Civilization.Rome)
    val token: String = cu._1
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    var res = WaitingGames.findListOfWaitingGames(RR.RA)
    assert(res.map(_._1).contains(gameid))
    var res1 = WaitingGames.listofCurrentGames(RR.RA)
    println(res1)
    // should be on waiting list, not current in play
    assert(!res.contains(gameid))
    val ctoken: String = II.joinGame(gameid, "China")
    println(ctoken)
    res = WaitingGames.findListOfWaitingGames(RR.RA)
    println(res)
    // should be removed from waiting list
    assert(!res.map(_._1).contains(gameid))
    // should be on the current game
    res1 = WaitingGames.listofCurrentGames(RR.RA)
    assert(res1.contains(gameid))

    val gamerome: CurrentGame = RR.RA.getCurrentGame(token)
    val gamechina: CurrentGame = RR.RA.getCurrentGame(ctoken)
    // should be the same game now
    gamerome.gameid should equal(gamechina.gameid)
  }

  private def activeciv(token : String, civ : String, phase : String): Unit = {
    val s = II.getData(II.GETBOARDGAME,token)
//        println(s)
    val j : JsValue = toJ(s)
    //println(j)
    // both should be active at the beginning
    val c = (j \ "board"\ "game" \ "active").as[String]
//    println(c)
    assert(c == civ)
    val p = (j \ "board"\ "game" \ "phase").as[String]
    assert(p == phase)
  }

  test("Two players game") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME2.json", Civilization.Rome)
    val token: String = cu._1
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = II.joinGame(gameid, "China")
    val b: GameBoard = I.getBoardForToken(token)
    var a: Seq[Command.T] = allowedCommands(b, Civilization.Rome)
    println(a)
    assert(a.contains(Command.SETCAPITAL))
    a = allowedCommands(b, Civilization.China)
    println(a)
    assert(a.contains(Command.SETCAPITAL))
    activeciv(token,"Rome","StartOfTurn")
    activeciv(ctoken,"China","StartOfTurn")
    var s: String = executeCommand(token, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    assert (s == null)
    // Rome completed already
    activeciv(token,"China","StartOfTurn")
    activeciv(ctoken,"China","StartOfTurn")
    s = executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    assert (s == null)
    // now trade, both are active
    activeciv(token,"Rome","Trade")
    activeciv(ctoken,"China","Trade")
    s = executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Trade\"")
    assert (s == null)
    s = executeCommand(token, "ENDOFPHASE", -1, -1, "\"Trade\"")
    assert (s == null)
    // Rome active
    activeciv(token,"Rome","CityManagement")
    // Rome active, not China
    activeciv(ctoken,"Rome","CityManagement")
    s = executeCommand(token, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    assert (s == null)
    // China active
    activeciv(token,"China","CityManagement")
    // China active, not China
    activeciv(ctoken,"China","CityManagement")
    s = executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now movement
    // Rome active
    activeciv(token,"Rome","Movement")
    // Rome active, not China
    activeciv(ctoken,"Rome","Movement")
  }

  test("Two players game, set capital") {
    val c = Helper.ReadAndPlayForTwo("test10/BOARDGAME2.json", "test10/GAME1.json", Civilization.Rome, Civilization.China)
    val tokeng = c._1
    val tokenc = c._2
    val b: GameBoard = I.getBoardForToken(tokenc)
    var a: Seq[Command.T] = allowedCommands(b, Civilization.Rome)
    println(a)
    assert(!a.contains(Command.SETCAPITAL))
    assert(a.contains(Command.SETARMY))
    a = allowedCommands(b, Civilization.China)
    assert(!a.contains(Command.SETCAPITAL))
    assert(a.contains(Command.SETARMY))
  }

  test("Two players game, set capital, scout and army") {
    val c = Helper.ReadAndPlayForTwo("test10/BOARDGAME2.json", "test10/GAME2.json", Civilization.Rome, Civilization.China)
    val tokeng = c._1
    val tokenc = c._2
    val b: GameBoard = I.getBoardForToken(tokenc)
    var a: Seq[Command.T] = allowedCommands(b, Civilization.Rome)
    println(a)
    a = allowedCommands(b, Civilization.China)
    println(a)
  }
}
