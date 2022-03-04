package civilization.test

import civilization.I.{CurrentGame, getBoardForToken}
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.io.readdir.{readGameBoard, readPlay, readTestJSON, readTestS}
import civilization.objects.{Civilization, CommandValues, _}
import play.api.libs.json._
import civilization.io.fromjson.toJ
import civilization.II.factory._
import civilization.action.{Command, constructCommand}
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.tojson.writeCommandValues

object Helper {


  val II = Factory.getI
  val RA = Factory.getR
  RA.getConn.setConnection("thinkarek", 6379, 1)
//  RA.getConn.setConnection("localhost", 6379, 0)
  II.setR(RA)

  //def I = {
  //}

  def X = {}

  private def getBoardS(path: String): String = readTestS("testmap/tiles/" + path)

  def getBoard(path: String): GameBoard = {

    val l: JsValue = toJ(getBoardS(path))
    readGameBoard(l)
  }

  private def getPlayS(path: String): String = readTestS("testmap/tiles/" + path)

  private def getPlay(path: String): Seq[CommandValues] = {
    val l: JsValue = toJ(getPlayS(path))
    readPlay(l)
  }

  def getBoardAndRegister(boardpath: String, civ: Civilization.T): (String, GameBoard) = {
    val g: GameBoard = getBoard(boardpath)
    val (token: String, gameid : Int) = civilization.I.decodeS(civilization.I.registerGame(g, civ))
    (token, g)
  }

  private def extractToken(s: String): String = s.split(",")(0)

  def registerOwner(civ: String): String =
    extractToken(II.getData(II.REGISTEROWNER, civ))

  def registerOwnerTwo(civ: String): String =
    extractToken(II.getData(II.REGISTEROWNERTWOGAME, civ))

  private def toSingleString(board: String, play: String): String = {
    // remove {} from play
    val i1: Int = play.indexOf("{") // the first {
    val i2: Int = play.length - play.reverse.indexOf("}")
    val p: String = play.slice(i1 + 1, i2 - 1).trim()
    val s: String = "{ \"" + S.board + "\" : " + board + "," + p + "}"
    s
  }


  def readBoardAndPlayT(boardpath: String, playPath: String, civ: Civilization.T): (String, GameBoard) = {
    val board: String = getBoardS(boardpath)
    val play: String = getPlayS(playPath)
    val t : String = II.readPlayerGameS(toSingleString(board,play),civ.toString())
    val token = extractToken(t)
    (token, getBoardForToken(token))
  }

  def getBoardToken(token : String) : GameBoard = getBoardForToken(token)

  def readGameSingle(gamepath : String, civ : Civilization.T) : (String, Int) = {
    val game : String = getBoardS(gamepath)
    val t : String = II.readPlayerGameS(game,civ.toString())
    val tk = t.split(',') // token
    (tk(0), tk(1).toInt)
  }

  def ReadAndPlayForTwo(boardpath: String, playPath: String, civ1: Civilization.T, civ2: Civilization.T): (String, String, Int) = {
    val board: String = getBoardS(boardpath)
    val play: String = getPlayS(playPath)
    val t : String = II.readPlayerGameS(toSingleString(board,play),civ1.toString() + ',' + civ2.toString())
    val tk = t.split(',') // token
    (tk(0), tk(1), tk(2).toInt)
  }

  def getLimitsH(gg: GameBoard, civ: Civilization.T): PlayerLimits = getLimits(gg, gg.playerDeck(civ))

  def allowedCommandsH(gg: GameBoard, civ: Civilization.T): Seq[Command.T] = allowedCommands(gg, gg.playerDeck(civ))

  def numberofTradeCalculateH(b: GameBoard, civ: Civilization.T): TradeForCivCalculate =
    numberofTradeCalculate(b, b.playerDeck(civ))

  def numberofTradeH(b: GameBoard, civ: Civilization.T): TradeForCiv =
    numberofTrade(b, b.playerDeck(civ))

  def getProductionForCityH(b: GameBoard, civ: Civilization.T, p: P): ProdForCity =
    getProductionForCity(b, b.playerDeck(civ), p)


  private def e(token: String, action: String, row: Int, col: Int, jsparam: String): String = {
    val s: String = II.executeCommand(token, action, row, col, jsparam)
    if (s != null) println(s)
    s
  }

  def executeCommandH(token: String, action: String, row: Int = -1, col: Int = -1, jsparam: String = null): Unit = {
    assert(e(token, action, row, col, jsparam) == null)
  }

  def executeCommandFail(token: String, action: String, row: Int = -1, col: Int = -1, jsparam: String = null): Unit = {
    assert(e(token, action, row, col, jsparam) != null)
  }

  // TEST, battle

  def getBattle(j: JsValue): JsValue =
    (j \ "board" \ "battle").get


  def checkendofbattle(j: JsValue, expected: Boolean): Unit = {
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

  def numof(g: GameBoard, u: CombatUnitType.T): Int = g.market.units.filter(_.utype == u).length

  def numof(g: GameBoard, civ: Civilization.T, u: CombatUnitType.T): Int =
    g.playerDeck(civ).units.filter(_.utype == u).length


  def activeciv(token: String, civ: String, phase: String): Unit = {
    val s = II.getData(II.GETBOARDGAME, token)
    //        println(s)
    val j: JsValue = toJ(s)
    //println(j)
    // both should be active at the beginning
    val c = (j \ "board" \ "game" \ "active").as[String]
    //    println(c)
    println(civ + " ? " + c)
    assert(c == civ)
    val p = (j \ "board" \ "game" \ "phase").as[String]
    println(phase + " ? " + p)
    assert(p == phase)
  }

  def jyou(j: JsValue): JsValue =
    (j \ "board" \ "you").as[JsValue]

  def jmap(j: JsValue): JsArray =
    (j \ "board" \ "map").as[JsArray]

  def jresources(j: JsValue): JsArray =
    (j \ "board" \ "resources").as[JsArray]
}


