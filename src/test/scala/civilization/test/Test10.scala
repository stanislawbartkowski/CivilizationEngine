package civilization.test

import civilization.I.{CurrentGame, II, WaitingGames, executeCommand}
import civilization.gameboard.GameBoard
import civilization.io.fromjson.toJ
import civilization.objects._
import civilization.{I, RR}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import play.api.libs.json.{JsArray, JsValue}
import Helper.{II,RA}
import Helper._


class Test10 extends AnyFunSuite {

  Helper.X

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

  test("Waiting for player to join") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME1.json", Civilization.Rome)
    val token: String = cu._1
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    assert(!II.allPlayersReady(token))
    var s : String = II.getData(II.LISTOFWAITINGGAMES)
    println(s)
    val (ctoken: String,_ : Int) = civilization.I.decodeS(II.joinGame(gameid, "China"))
    // joined, game ready
    assert(II.allPlayersReady(token))
    assert(II.allPlayersReady(ctoken))
    s  = II.getData(II.LISTOFWAITINGGAMES)
    println(s)
    val w : JsArray = toJ(s).as[JsArray]
//    val found : Boolean = w.value.find(j => j
    w.value.foreach(println)
    assert (! w.value.map(j => (j \ "gameid").as[Int]).toSet.contains(gameid))
    // now deregister China
    II.getData(II.UNREGISTERTOKEN,ctoken)
    // verify that players not ready for America again
    assert(!II.allPlayersReady(token))
    II.deleteGame(gameid)
  }


  test("Read two player game") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME1.json", Civilization.Rome)
    val token: String = cu._1
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    var res = WaitingGames.findListOfWaitingGames(RA)
    assert(res.map(_.gameid).contains(gameid))
    var res1 = WaitingGames.listofCurrentGames(RA)
    println(res1)
    // should be on waiting list, not current in play
    assert(!res.contains(gameid))
    val ctoken: String = civilization.I.decodeS(II.joinGame(gameid, "China"))._1
    println(ctoken)
    res = WaitingGames.findListOfWaitingGames(RA)
    println(res)
    // should be removed from waiting list
    assert(!res.map(_.gameid).contains(gameid))
    // should be on the current game
    res1 = WaitingGames.listofCurrentGames(RA)
    assert(res1.contains(gameid))

    val gamerome: CurrentGame = RA.getCurrentGame(token)
    val gamechina: CurrentGame = RA.getCurrentGame(ctoken)
    // should be the same game now
    gamerome.gameid should equal(gamechina.gameid)
    II.deleteGame(gameid)
  }

  test("Two players game") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME2.json", Civilization.Rome)
    val token: String = cu._1
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(gameid)
    val ctoken: String = civilization.I.decodeS(II.joinGame(gameid, "China"))._1
    val b: GameBoard = I.getBoardForToken(token)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.contains(Command.SETCAPITAL))
    a = allowedCommandsH(b, Civilization.China)
    println(a)
//    assert(a.contains(Command.SETCAPITAL))
    assert(a.isEmpty)
    Helper.activeciv(token, "Rome", "StartOfTurn")
    // 2017/12/28, StartOfTurn in turn
    // Helper.activeciv(ctoken, "China", "StartOfTurn")
    Helper.activeciv(ctoken, "Rome", "StartOfTurn")
    var s: String = executeCommand(token, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    assert(s == null)
    // Rome completed already
    Helper.activeciv(token, "China", "StartOfTurn")
    Helper.activeciv(ctoken, "China", "StartOfTurn")
    s = executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"StartOfTurn\"")
    assert(s == null)
    // now trade, both are active
    Helper.activeciv(token, "Rome", "Trade")
    Helper.activeciv(ctoken, "China", "Trade")
    s = executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"Trade\"")
    assert(s == null)
    s = executeCommand(token, "ENDOFPHASE", -1, -1, "\"Trade\"")
    assert(s == null)
    // Rome active
    Helper.activeciv(token, "Rome", "CityManagement")
    // Rome active, not China
    Helper.activeciv(ctoken, "Rome", "CityManagement")
    s = executeCommand(token, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    assert(s == null)
    // China active
    Helper.activeciv(token, "China", "CityManagement")
    // China active, not China
    Helper.activeciv(ctoken, "China", "CityManagement")
    s = executeCommand(ctoken, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now movement
    // Rome active
    Helper.activeciv(token, "Rome", "Movement")
    // Rome active, not China
    Helper.activeciv(ctoken, "Rome", "Movement")
    II.deleteGame(gameid)
  }

  test("Two players game, set capital") {
    val c = Helper.ReadAndPlayForTwo("test10/BOARDGAME2.json", "test10/GAME1.json", Civilization.Rome, Civilization.China)
    val tokeng = c._1
    val tokenc = c._2
    val b: GameBoard = I.getBoardForToken(tokenc)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(!a.contains(Command.SETCAPITAL))
    assert(a.contains(Command.SETARMY))
    a = allowedCommandsH(b, Civilization.China)
    assert(!a.contains(Command.SETCAPITAL))
    assert(!a.contains(Command.SETARMY))
  }

  test("Two players game, set capital, scout and army") {
    val c = Helper.ReadAndPlayForTwo("test10/BOARDGAME2.json", "test10/GAME2.json", Civilization.Rome, Civilization.China)
    val tokeng = c._1
    val tokenc = c._2
    val b: GameBoard = I.getBoardForToken(tokenc)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    a = allowedCommandsH(b, Civilization.China)
    println(a)
  }

  test("Check two players game") {
//    val token : String = II.getData(II.REGISTEROWNERTWOGAME,"Rome,China")
    val token : String = registerOwnerTwo("Rome,China")
    println(token)
    val res1 : Seq[civilization.I.WaitingGames.WaitingGames] = WaitingGames.findListOfWaitingGames(RA)
    println(res1)
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    println(game)
    println(gameid)
    val p = res1.find(_.gameid == gameid).get
    println(p)
    println(p.registeredplayers)
    assert(p.registeredplayers.contains(Civilization.Rome))
    assert(p.waiting.contains(Civilization.China))
    val j = II.getData(II.LISTOFWAITINGGAMES)
    println(j)
  }

}
