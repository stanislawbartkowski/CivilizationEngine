package civilization.test

import civilization.I.CurrentGame
import civilization.I.executeCommand
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.io.readdir.{readGameBoard, readPlay, readTestJSON}
import civilization.message.{FatalError, Mess}
import civilization.objects.{Civilization, CommandValues, _}
import play.api.libs.json._
import civilization.io.fromjson.toJ
import civilization.II.factory._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._

object Helper {


  //  RR.setConnection("localhost", 6379, 1)
  //    R.setConnection("redis://localhost:6379")
  //  civilization.I.setR(RR.RA)
  val II = Factory.getI
  val RA = Factory.getR
  RA.getConn.setConnection("localhost", 6379, 1)
  II.setR(RA)

  def I = {
    //    RR.setConnection("localhost", 6379, 1)
    //    R.setConnection("redis://localhost:6379")
    //    civilization.I.setR(RR.RA)
  }

  def getBoard(path: String): GameBoard = {

    val l: JsValue = readTestJSON("resources/map/tiles/" + path)
    //    println(l)
    readGameBoard(l)
  }

  private def getPlay(path: String): Seq[CommandValues] = {
    val l: JsValue = readTestJSON("resources/map/tiles/" + path)
    readPlay(l)
  }

  def getBoardAndRegister(boardpath: String, civ: Civilization.T): (String, GameBoard) = {
    val g: GameBoard = getBoard(boardpath)
    val token: String = civilization.I.registerGame(g, civ)
    (token, g)
  }

  private def executeC(gb: (CurrentGame, GameBoard), com: CommandValues) = {
    val m: Mess = executeCommand(gb, com, true)
    if (m != null) throw new FatalError(m)
  }

  def readBoardAndPlayT(boardpath: String, playPath: String, civ: Civilization.T): (String, GameBoard) = {
    val gg = getBoardAndRegister(boardpath, civ)
    val g: GameBoard = gg._2
    val token: String = gg._1
    val p: Seq[CommandValues] = getPlay(playPath)
    val game: CurrentGame = RA.getCurrentGame(token)
    p.foreach(co => executeC((game, g), co))
    (token, g)
  }

  def readBoardAndPlay(boardpath: String, playPath: String, civ: Civilization.T): GameBoard = readBoardAndPlayT(boardpath, playPath, civ)._2

  def ReadAndPlayForTwo(boardpath: String, playPath: String, civ1: Civilization.T, civ2: Civilization.T): (String, String) = {
    val cu = readBoardAndPlayT(boardpath, playPath, civ1)
    val token: String = cu._1
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    val ctoken: String = II.joinGame(gameid, civ2.toString)
    return (token, ctoken)
  }

  def getLimitsH(gg: GameBoard, civ: Civilization.T): PlayerLimits = getLimits(gg, gg.playerDeck(civ))

  def allowedCommandsH(gg: GameBoard, civ: Civilization.T): Seq[Command.T] = allowedCommands(gg, gg.playerDeck(civ))

  def numberofTradeCalculateH(b: GameBoard, civ: Civilization.T): TradeForCivCalculate =
    numberofTradeCalculate(b, b.playerDeck(civ))

  def numberofTradeH(b: GameBoard, civ: Civilization.T): TradeForCiv =
    numberofTrade(b, b.playerDeck(civ))

  private def e(token: String, action: String, row: Int, col: Int, jsparam: String): String = {
    val s: String = II.executeCommand(token, action, row, col, jsparam)
    if (s != null) println(s)
    s
  }

  def executeCommandH(token: String, action: String, row: Int, col: Int, jsparam: String = null): Unit = {
    assert(e(token, action, row, col, jsparam) == null)
  }

  def executeCommandFail(token: String, action: String, row: Int, col: Int, jsparam: String): Unit = {
    assert(e(token, action, row, col, jsparam) != null)
  }

  // TEST, battle

  def getBattle(j: JsValue): JsValue =
    (j \ "board" \ "battle").get


  def checkendofgame(j: JsValue, expected: Boolean): Unit = {
    val t: Boolean = (j \ "endofbattle").get.as[Boolean]
    println(t)
    assert(expected == t)
  }

  def checkattackerwinner(j: JsValue, expected: Boolean): Unit = {
    val t: Boolean = (j \ "attackerwinner").get.as[Boolean]
    println(t)
    assert(expected == t)
  }

  def verifyiron(token: String, canuse: Boolean): Unit = {
    val s = II.getData(II.GETBOARDGAME, token)
    val js: JsValue = toJ(s)
    val batt: JsValue = (js \ "board" \ "battle").get
    println(batt)
    val attacker = (batt \ "attacker").get
    println(attacker)
    val canuseiron = (attacker \ "canuseiron").as[Boolean]
    println(canuseiron)
    // can use iron
    assert(canuseiron == canuse)

  }

  def getB(token: String) = {
    val s = II.getData(II.GETBOARDGAME, token)
    toJ(s)
  }

  def activeciv(token: String, civ: String, phase: String): Unit = {
    val s = II.getData(II.GETBOARDGAME, token)
    //        println(s)
    val j: JsValue = toJ(s)
    //println(j)
    // both should be active at the beginning
    val c = (j \ "board" \ "game" \ "active").as[String]
    //    println(c)
    assert(c == civ)
    val p = (j \ "board" \ "game" \ "phase").as[String]
    assert(p == phase)
  }

  def jyou(j: JsValue): JsValue =
    (j \ "board" \ "you").as[JsValue]

  def jmap(j: JsValue): JsArray =
    (j \ "board" \ "map").as[JsArray]

  def jresources(j: JsValue): JsArray =
    (j \ "board" \ "resources").as[JsArray]

}

