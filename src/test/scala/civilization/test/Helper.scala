package civilization.test

import civilization.I.{CurrentGame}
import civilization.I.{executeCommand}
import civilization.RR
import civilization.gameboard.GameBoard
import civilization.io.readdir.{readGameBoard, readPlay, readTestJSON}
import civilization.message.{FatalError, Mess}
import civilization.objects.{CommandValues, _}
import play.api.libs.json.JsValue
import civilization.I.II

object Helper {

  def I = {
    RR.setConnection("localhost", 6379, 1)
    //    R.setConnection("redis://localhost:6379")
    civilization.I.setR(RR.RA)
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
    val m: Mess = executeCommand(gb, com,true)
    if (m != null) throw new FatalError(m)
  }

  def readBoardAndPlayT(boardpath: String, playPath: String, civ: Civilization.T): (String, GameBoard) = {
    val gg = getBoardAndRegister(boardpath, civ)
    val g: GameBoard = gg._2
    val token: String = gg._1
    val p: Seq[CommandValues] = getPlay(playPath)
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    p.foreach(co =>  executeC((game, g), co))
    (token, g)
  }

  def readBoardAndPlay(boardpath: String, playPath: String, civ: Civilization.T): GameBoard = readBoardAndPlayT(boardpath, playPath, civ)._2

  def ReadAndPlayForTwo(boardpath: String, playPath: String, civ1: Civilization.T, civ2: Civilization.T): (String, String) = {
    val cu = readBoardAndPlayT(boardpath, playPath, civ1)
    val token: String = cu._1
    val game: CurrentGame = RR.RA.getCurrentGame(token)
    val gameid: Int = game.gameid
    val ctoken: String = II.joinGame(gameid, civ2.toString)
    return (token, ctoken)
  }

  private def e(token: String, action: String, row: Int, col: Int, jsparam: String) : String = {
    val s : String = II.executeCommand(token,action,row,col,jsparam)
    if (s != null) println(s)
    s
  }

  def executeCommandH(token: String, action: String, row: Int, col: Int, jsparam: String): Unit = {
    assert (e(token,action,row,col,jsparam) == null)
  }

  def executeCommandFail(token: String, action: String, row: Int, col: Int, jsparam: String): Unit = {
    assert (e(token,action,row,col,jsparam) != null)
  }

}

