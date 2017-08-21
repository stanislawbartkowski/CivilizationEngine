package civilization.test

import civilization.action.Play.Play
import civilization.gameboard.GameBoard
import civilization.io.readdir.{readGameBoard, readTestJSON,readPlay}
import play.api.libs.json.JsValue
import civilization.helper.playList

object Helper {

  def getBoard(path: String) : GameBoard = {

    val l: JsValue = readTestJSON("resources/map/tiles/" + path)
//    println(l)
    readGameBoard(l)
  }

  def getPlay(path:String) : Play = {
    val l: JsValue = readTestJSON("resources/map/tiles/" + path)
    readPlay(l)
  }

  def readBoardAndPlay(boardpath : String, playPath : String) : GameBoard = {
    val g: GameBoard = getBoard(boardpath)
    val p : Play = getPlay(playPath)
//    g.play = p
    playList(g,p)
    g
  }
}

