package civilization.test

import civilization.I.{CurrentGame, getBoardForToken}
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.io.readdir.{readGameBoard, readPlay, readTestJSON}
import civilization.message.{FatalError, Mess}
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
  RA.getConn.setConnection("think", 6379, 1)
  II.setR(RA)

  def I = {
  }

  def getBoard(path: String): GameBoard = {

    val l: JsValue = readTestJSON("testmap/tiles/" + path)
    //    println(l)
    readGameBoard(l)
  }

  private def getPlay(path: String): Seq[CommandValues] = {
    val l: JsValue = readTestJSON("testmap/tiles/" + path)
    readPlay(l)
  }

  def getBoardAndRegister(boardpath: String, civ: Civilization.T): (String, GameBoard) = {
    val g: GameBoard = getBoard(boardpath)
    val token: String = civilization.I.registerGame(g, civ)._1
    (token, g)
  }

  private def extractToken(s : String) : String = s.split(",")(0)

  def registerOwner(civ : String) : String =
     extractToken(II.getData(II.REGISTEROWNER, civ))

  def registerOwnerTwo(civ : String) : String =
    extractToken(II.getData(II.REGISTEROWNERTWOGAME, civ))

  //  val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")


  def readBoardAndPlayT(boardpath: String, playPath: String, civ: Civilization.T): (String, GameBoard) = {
    val g: GameBoard = getBoard(boardpath)
    val p: Seq[CommandValues] = getPlay(playPath)
    p.foreach(c => {
      val co: Command = constructCommand(c)
      g.play.addCommand(co)
    })
    val token: String = civilization.I.registerGame(g, civ)._1
    (token, getBoardForToken(token))
  }

  def ReadAndPlayForTwo(boardpath: String, playPath: String, civ1: Civilization.T, civ2: Civilization.T): (String, String, Int) = {
    val cu = readBoardAndPlayT(boardpath, playPath, civ1)
    val token: String = cu._1
    val game: CurrentGame = RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    val ctoken: String = II.joinGame(gameid, civ2.toString)
    return (token, ctoken, gameid)
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

  def executeCommandFail(token: String, action: String, row: Int, col: Int, jsparam: String): Unit = {
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


