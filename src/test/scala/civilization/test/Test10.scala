package civilization.test

import org.scalatest.FunSuite
import org.scalatest._
import Matchers._
import civilization.objects._
import civilization.I.{CurrentGame, WaitingGames}
import civilization.RR
import civilization.II

class Test10 extends FunSuite {

  Helper.I

  test("Register and unregister") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME1.json",Civilization.Rome)
    val token : String = cu._1
    val game : String = II.getData(II.GETBOARDGAME,token)
    println(game)
    //  unregister
    II.getData(II.UNREGISTERTOKEN,token)
    // should throw exception
    an [Exception] should be thrownBy II.getData(II.GETBOARDGAME, token)
  }


  test("Read two player game") {
    val cu = Helper.getBoardAndRegister("test10/BOARDGAME1.json",Civilization.Rome)
    val token : String = cu._1
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    val gameid : Int = game.gameid
    println(gameid)
    var res = WaitingGames.findListOfWaitingGames(RR.RA)
    assert(res.map(_._1).contains(gameid))
    var res1 = WaitingGames.listofCurrentGames(RR.RA)
    println(res1)
    // should be on waiting list, not current in play
    assert(! res.contains(gameid))
    val ctoken : String = II.joinGame(gameid,"China")
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
    gamerome.gameid should equal (gamechina.gameid)
  }

}
