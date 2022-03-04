package civilization.test

import civilization.I.{II, _}
import civilization.gameboard.{Figures, GameBoard}
import civilization.helper._
import civilization.helper.move.MoveItemize
import civilization.io.fromjson._
import civilization.objects._
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json._
import civilization.helper.move.MoveItemize.{PossibleMove, itemizeForMove}
import Helper._

class Test9 extends AnyFunSuite {

  Helper.X

  test("Execute command, set army and scout goto citymanagement") {
    val r = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME1.json", Civilization.Rome)
    val token: String = r._1
    var b: GameBoard = r._2
    val cu: CurrentPhase = currentPhase(b)
    println(cu)
    assert(cu.turnPhase == TurnPhase.CityManagement)
    val prod: Int = getProductionForCityH(b,  Civilization.Rome, P(1, 2)).prod
    println(prod)
    var l: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(l)
    var s: String = executeCommand(token, "BUYARMY", 1, 2, "{\"col\" : 2, \"row\" : 2}")
    assert(s == null)
    b = getBoardForToken(token)
    l = allowedCommandsH(b, Civilization.Rome)
    println(l)
    // only end of phase
    assert(l.find(_ == Command.BUYSCOUT).isEmpty)
    assert(l.find(_ == Command.BUYARMY).isEmpty)

  }

  test("Execute command, buy scout") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME1.json", Civilization.Rome)
    val s: String = II.getData(GETBOARDGAME, token)
    println(s)
    val board: JsValue = toJ(s)
    val map: JsArray = (board \ "board" \ "map").get.as[JsArray]
    var foundScout: Boolean = false
    var foundArmy: Boolean = false
    map.value.foreach(v => v.as[JsArray].value.foreach(s => {
      //println(s)
      val army: Int = (s \ "numberofArmies").get.as[Int]
      val scout: Int = (s \ "numberofScouts").get.as[Int]
      if (army > 0) foundArmy = true
      if (scout > 0) foundScout = true
    }))
    assert(foundArmy)
    assert(foundScout)
    val a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)

  }

  test("Execute command, movement start") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME2.json", Civilization.Rome)
//    val token: String = registerGame(b, Civilization.Rome);
    val a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.find(_ == Command.STARTMOVE).isDefined)
    var i: Seq[(Figures, P)] = MoveItemize.itemizeforStartOfMove(b, Civilization.Rome)
    println(i)
    assert(i.length == 2)
    val s: String = II.itemizeCommand(token, "STARTMOVE")
    println(s)
  }

  test("Execute command, movement started") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME3.json", Civilization.Rome)
    //val token: String = registerGame(b, Civilization.Rome);
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.find(_ == Command.MOVE).isDefined)
    assert(a.find(_ == Command.ENDOFMOVE).isDefined)
    assert(a.find(_ == Command.REVEALTILE).isEmpty)
    var s: String = executeCommand(token, "MOVE", 1, 2, null)
    println(s)
    assert(s == null)
    b = getBoardForToken(token)
    a = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.find(_ == Command.MOVE).isDefined)
    // does not contains end of move, cannot stop in city
    assert(!(a contains Command.ENDOFMOVE))
    assert(a.find(_ == Command.REVEALTILE).isEmpty)
    s = executeCommand(token, "MOVE", 2, 2, null)
    println(s)
    assert(s == null)
    b = getBoardForToken(token)
    a = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.find(_ == Command.MOVE).isEmpty)
    assert(a.find(_ == Command.ENDOFMOVE).isDefined)
    assert(a.find(_ == Command.REVEALTILE).isEmpty)
    s = executeCommand(token, "ENDOFMOVE", -1, -1, null)
    println(s)
    assert(s == null)
    // next point
    b = getBoardForToken(token)
    a = allowedCommandsH(b, Civilization.Rome)
    println(a)
    // start armie
    // { "numberofArmies" : 0, "numberofScouts" : 1}
    s = executeCommand(token, "STARTMOVE", 0, 1, "{ \"numberofArmies\" : 1, \"numberofScouts\" : 0}")
    println(s)
    assert(s == null)
    s = executeCommand(token, "MOVE", 1, 1, null)
    println(s)
    assert(s == null)
    // itemize
    s = II.itemizeCommand(token, "MOVE")
    println(s)
    b = getBoardForToken(token)
    val o: Option[PossibleMove] = itemizeForMove(b, b.playerDeck(Civilization.Rome))
    println(o.get.move)
    assert(o.get.move.find(_ == P(0, 1)).isDefined)
    assert(o.get.move.find(_ == P(2, 1)).isDefined)
    assert(o.get.move.find(_ == P(1, 0)).isDefined)
    // city cannot be the last
    assert(o.get.move.find(_ == P(1, 2)).isEmpty)
    s = executeCommand(token, "MOVE", 2, 1, null)
    println(s)
    assert(s == null)
  }

  test("Execute command, next movements") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME4.json", Civilization.Rome)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.find(_ == Command.MOVE).isDefined)
    assert(a.find(_ == Command.ENDOFMOVE).isDefined)
    assert(a.find(_ == Command.REVEALTILE).isDefined)
    var o: Option[PossibleMove] = itemizeForMove(b, b.playerDeck(Civilization.Rome))
    println(o.get.move)
    println(o.get.reveal)
    assert(o.get.reveal.length == 1)
    assert(o.get.reveal.head._1 == P(1, 0))
    assert(o.get.reveal.head._2 == Orientation.Down)
    var s: String = executeCommand(token, "MOVE", 4, 1, null)
    println(s)
    assert(s != null)
    s = executeCommand(token, "REVEALTILE", 1, 0, "\"Down\"")
    println(s)
    // move again
    s = executeCommand(token, "MOVE", 4, 1, null)
    println(s)
    assert(s != null)
    // check move
    b = getBoardForToken(token)
    val last: Seq[PlayerMove] = civLastMoves(b, Civilization.Rome)
    println(last)
    assert(last.last.lastp == P(3, 1))
    o = itemizeForMove(b, b.playerDeck(Civilization.Rome))
    // only end of move
    assert(o.get.endofmove)
    //    println(o.get.move)
    //    println(o.get.reveal)
    a = allowedCommandsH(b, b.playerDeck(Civilization.Rome))
    println(a)
    // only end of move
    assert(a.length == 1 && a.head == Command.ENDOFMOVE)
    // end of move
    s = executeCommand(token, "ENDOFMOVE", -1, -1, null)
    assert(s == null)
    b = getBoardForToken(token)
    a = allowedCommandsH(b, Civilization.Rome)
    println(a)
    // start move
    assert(a.find(_ == Command.STARTMOVE).isDefined)
    s = executeCommand(token, "STARTMOVE", 2, 2, "{ \"numberofArmies\" : 0, \"numberofScouts\" : 1}")
    println(s)
    assert(s == null)
    s = executeCommand(token, "MOVE", 3, 2, null)
    println(s)
    assert(s == null)
    b = getBoardForToken(token)
    a = allowedCommandsH(b, Civilization.Rome)
    println(a)
    // not REVEALTIME, is revealed already
    assert(a.find(_ == Command.REVEALTILE).isEmpty)
    // move to next tile
    s = executeCommand(token, "MOVE", 4, 2, null)
    println(s)
    assert(s == null)
  }

  test("Execute command, set up next city") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME5.json", Civilization.Rome)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.find(_ == Command.SETCITY).isDefined)
    var s: String = executeCommand(token, "SETCITY", 4, 2, null)
    println(s)
    b = getBoardForToken(token)
    val ci: Seq[MapSquareP] = citiesForCivilization(b, Civilization.Rome)
    println(ci)
    assert(ci.length == 2)
    var trade: Int = numberofTradeH(b, Civilization.Rome).trade
    println(trade)
    assert(14 == trade )
    val figures: Seq[MapSquareP] = getFigures(b, Civilization.Rome)
    figures.foreach(println)
    assert(figures.length == 1)
    var count: (Int, Int) = getNumberOfArmies(b, Civilization.Rome)
    println(count)
    println("=======================")
    assert(count._1 == 1)
    assert(count._2 == 0)
  }

  test("Execute command, two city actions") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME6.json", Civilization.Rome)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    var s: String = executeCommand(token, "BUYARMY", 1, 2, "{\"col\" : 2, \"row\" : 2}")
    assert(s == null)
    s = executeCommand(token, "BUYSCOUT", 4, 2, "{\"col\" : 2, \"row\" : 5}")
    assert(s == null)
    b = getBoardForToken(token)
    a = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.length == 1)
  }

  test("Execute command, check movement to starting point") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME3.json", Civilization.Rome)
    val curr: Option[PlayerMove] = getCurrentMove(b, Civilization.Rome)
    println(curr)
    assert(curr.get.lastp == P(0, 2))
    var s: String = executeCommand(token, "MOVE", 1, 3, null)
    println(s)
    assert(s != null)
    // movement to the same point
    s = executeCommand(token, "MOVE", 0, 2, null)
    println(s)
    assert(s != null)
    s = executeCommand(token, "MOVE", 1, 2, null)
    println(s)
    assert(s == null)
  }

  test("Execute command, revealtime itemize") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME4.json", Civilization.Rome)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    // get reveal itemized
    val s: String = II.itemizeCommand(token, "REVEALTILE")
    println(s)
    assert(s != null)
    // TODO: more detailed test
  }

  test("Check itemize for setcapital") {
//    val token: String = II.getData(REGISTEROWNER, "Germany")
    val token : String = registerOwner("Germany")

    val s: String = II.itemizeCommand(token, "SETCAPITAL")
    println(s)
    assert(s != null)
    val a: JsArray = Json.parse(s).as[JsArray]
    assert(a.value.length == 4)
  }

  test("Execute command, check itemized for SetCity") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME5.json", Civilization.Rome)
    val s: String = II.itemizeCommand(token, "SETCITY")
    println(s)
    assert(s != null)
    val a: JsArray = Json.parse(s).as[JsArray]
    println(a)
    assert(a.value.length == 1)
  }

  test("Execute command, check two figures ending on the sampe square") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME7.json", Civilization.Rome)
    var s: String = executeCommand(token, "STARTMOVE", 3, 1, "{ \"numberofArmies\" : 1, \"numberofScouts\" : 0}")
    println(s)
    assert(s == null)
    s = executeCommand(token, "MOVE", 4, 1, null)
    println(s)
    assert(s == null)
    s = executeCommand(token, "MOVE", 4, 2, null)
    println(s)
    assert(s == null)
    s = executeCommand(token, "ENDOFMOVE", -1, -1, null)
    println(s)
    assert(s == null)
  }

  test("Execute command, check two figures ending on the sampe square and available commands") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME8.json", Civilization.Rome)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
    assert(a.find(_ == Command.STARTMOVE).isEmpty)
  }

  test("Check list of games available") {
    var (token,b) = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test9/GAME8.json", Civilization.Rome)
    val js = II.getData(II.LISTOFGAMES)
    println(js)
    var a: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(a)
  }

}