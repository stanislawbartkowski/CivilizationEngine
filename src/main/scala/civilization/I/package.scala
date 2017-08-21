package civilization

import civilization.io.tojson._
import civilization.io.fromjson.toJ
import play.api.libs.json._
import java.security.SecureRandom
import java.math.BigInteger
import java.util.Calendar

import civilization.objects._
import civilization.action._
import civilization.helper._
import civilization.gameboard.GameBoard
import civilization.io.readdir.readListOfTiles
import civilization.message._
import civilization.io.readdir.GenBoard.genBoard

import scala.collection.mutable.Map


object I {

  val LISTOFCIV: Int = 0;
  val REGISTEROWNER: Int = 1
  val GETBOARDGAME: Int = 2

  private val random = new SecureRandom()

  private case class Board(val g: GameBoard, val civ: Civilization.T, var timestamp: Long)

  private val m: Map[String, Board] = Map()

  private def genToken(): String = new BigInteger(130, random).toString(32)

  def getData(what: Int, tokenorciv: String): String = {
    synchronized {
      what match {
        case LISTOFCIV => getListOfCiv()
        case REGISTEROWNER => registerOwnerPlay(tokenorciv)
        //        case GETBOARDGAME => getBoardName(tokenorciv)
        case GETBOARDGAME => getBoardForCiv(tokenorciv)
      }
    }
  }

  private def getListOfCiv(): String = {
    val tiles: Set[Civilization.T] = readListOfTiles.filter(_.tile.civhome).map(_.tile.civ).toSet
    Json.prettyPrint(writeListOfCiv(tiles contains _))
  }

  private def registerOwnerPlay(civ: String): String = {
    val v: Option[Civilization.T] = Civilization.values.find(_.toString == civ)
    val g: GameBoard = genBoard(List(v.get), "TEST1.json")
    registerGame(g, v.get)
  }

  def registerGame(g: GameBoard, civ: Civilization.T): String = {
    val token: String = genToken()
    m.put(token, Board(g, civ, Calendar.getInstance().getTime.getTime))
    token
  }


  private def getBoardName(token: String): String = {
    val g: GameBoard = m.get(token).get.g
    Json.prettyPrint(writesGameBoard(g))
  }

  private def getBoardForCiv(token: String): String = {
    val g: GameBoard = m.get(token).get.g
    val civ: Civilization.T = m.get(token).get.civ
    Json.prettyPrint(genboardj.genBoardGameJson(g, civ))
  }

  def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String = {
    val g: GameBoard = m.get(token).get.g
    val civ: Civilization.T = m.get(token).get.civ
    val command: Command.T = Command.withName(action)
    val co: Command = constructCommand(command, civ, if (row == -1) null else P(row, col), if (jsparam == null) null else toJ(jsparam))
    var mess: Mess = playCommand(g, co)
    return if (mess == null) return null else mess.toString
  }

  def itemizeCommand(token: String, action: String): String = {
    val command: Command.T = Command.withName(action)
    val b: Board = m.get(token).get
    AllowedCommands.itemizeCommandS(b.g, b.civ, command)
  }

  /* for test only */
  def getBoardForToken(token: String): GameBoard = m.get(token).get.g

}

