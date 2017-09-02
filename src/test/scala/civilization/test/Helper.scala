package civilization.test

import civilization.gameboard.GameBoard
import civilization.io.readdir.{readGameBoard, readPlay, readTestJSON}
import play.api.libs.json.JsValue
import civilization.RR
import civilization.objects.CommandValues
import civilization.objects._

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

  def readBoardAndPlayT(boardpath: String, playPath: String, civ: Civilization.T): (String, GameBoard) = {
    val g: GameBoard = getBoard(boardpath)
    val token: String = civilization.I.registerGame(g, civ)
    val p: Seq[CommandValues] = getPlay(playPath)
    p.foreach(co => civilization.I.executeCommand(token, co))
    (token, civilization.I.getBoardForToken(token))
  }

  def readBoardAndPlay(boardpath: String, playPath: String, civ: Civilization.T): GameBoard = readBoardAndPlayT(boardpath, playPath, civ)._2

}

