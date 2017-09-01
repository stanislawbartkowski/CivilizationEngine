package civilization

import civilization.io.tojson._
import civilization.io.fromjson._
import play.api.libs.json._
import java.security.SecureRandom
import java.math.BigInteger
import java.util.Calendar

import civilization.I.RAccess
import civilization.objects._
import civilization.action._
import civilization.helper._
import civilization.gameboard.GameBoard
import civilization.io.readdir.{readGameBoard, readListOfTiles}
import civilization.message._
import civilization.io.readdir.GenBoard.genBoard

package object I {

  private var r: RAccess = _

  def setR(r: RAccess) = {
    this.r = r
  }

  val LISTOFCIV: Int = 0;
  val REGISTEROWNER: Int = 1
  val GETBOARDGAME: Int = 2

  private val random = new SecureRandom()

  private def genToken(): String = new BigInteger(130, random).toString(32)

  private def getBoard(token: String): (CurrentGame, GameBoard) = {
    val game: CurrentGame = r.getCurrentGame(token)
    val s: String = r.getGame(game.gameid)
    val g: GameBoard = readGameBoard(toJ(s))
    // replay game
    val p: Seq[String] = r.getPlayForGame(game.gameid)
    p.foreach(s => {
      val co: CommandValues = toParams(toJ(s));
      val comm: Command = action.constructCommand(co)
      playsingleCommand(g, comm)
    }
    )
    (game, g)
  }

  private def toC(com: Command): CommandValues = CommandValues(com.command, com.civ, com.p, com.j)

  def getData(what: Int, tokenorciv: String): String = {
    synchronized {
      what match {
        case LISTOFCIV => getListOfCiv()
        case REGISTEROWNER => registerOwnerPlay(tokenorciv)
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
    val gameS: String = writesGameBoard(g).toString()
    val gameid: Int = r.registerGame(gameS)
    val cu: CurrentGame = CurrentGame(gameid, civ, Calendar.getInstance().getTime.getTime)
    //    m.put(token, Board(g, civ, Calendar.getInstance().getTime.getTime))
    // play
    g.play.commands.foreach(co => {
      val cc: CommandValues = toC(co)
      val s: String = writeCommandValues(cc).toString()
      r.addMoveToPlay(gameid, s)
    }
    )
    r.registerCurrentGame(token, cu)
    token
  }


  private def getBoardName(token: String): String = {
    val g = getBoard(token)
    Json.prettyPrint(writesGameBoard(g._2))
  }

  private def getBoardForCiv(token: String): String = {
    val g = getBoard(token)
    val civ: Civilization.T = g._1.civ
    Json.prettyPrint(genboardj.genBoardGameJson(g._2, civ))
  }

  private def executeCommand(gb: (CurrentGame, GameBoard), com: CommandValues): String = {
    val co: Command = constructCommand(com)
    var mess: Mess = playCommand(gb._2, co, c => {
      val cv: CommandValues = toC(c)
      r.addMoveToPlay(gb._1.gameid, writeCommandValues(cv).toString())
    })
    return if (mess == null) return null else mess.toString
  }

  // for testing only
  def executeCommand(token: String, com: CommandValues): String = executeCommand(getBoard(token), com)

  def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String = {
    val gb = getBoard(token)
    val g: GameBoard = gb._2
    val civ: Civilization.T = gb._1.civ
    val command: Command.T = Command.withName(action)
    val coma: CommandValues = CommandValues(command, civ, if (row == -1) null else P(row, col), if (jsparam == null) null else toJ(jsparam))
    executeCommand(gb, coma)
  }

  def itemizeCommand(token: String, action: String): String = {
    val command: Command.T = Command.withName(action)
    val g = getBoard(token)
    AllowedCommands.itemizeCommandS(g._2, g._1.civ, command)
  }

  /* for test only */
  def getBoardForToken(token: String): GameBoard = getBoard(token)._2

}

// to be visible from Java

object II {
  val LISTOFCIV = I.LISTOFCIV
  val REGISTEROWNER = I.REGISTEROWNER
  val GETBOARDGAME = I.GETBOARDGAME

  def getData(what: Int, tokenorciv: String): String = I.getData(what, tokenorciv)

  def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String = I.executeCommand(token, action, row, col, jsparam)

  def itemizeCommand(token: String, action: String): String = I.itemizeCommand(token, action)

  def setR(r: RAccess) = I.setR(r)

}
