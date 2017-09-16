package civilization

import java.math.BigInteger
import java.security.SecureRandom
import java.util.Calendar

import civilization.I.RAccess
import civilization.action._
import civilization.gameboard.{GameBoard, GameMetaData}
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.readdir.GenBoard.genBoard
import civilization.io.readdir.{readGameBoard, readListOfTiles}
import civilization.io.tojson._
import civilization.message._
import civilization.objects._
import play.api.libs.json._

package object I {

  private var r: RAccess = _

  def setR(r: RAccess) = {
    this.r = r
  }

  final val LISTOFCIV: Int = 0;
  final val REGISTEROWNER: Int = 1
  final val GETBOARDGAME: Int = 2
  final val LISTOFGAMES: Int = 3
  final val UNREGISTERTOKEN: Int = 4
  final val LISTOFWAITINGGAMES: Int = 5
  final val REGISTEROWNERTWOGAME: Int = 6

  private val random = new SecureRandom()

  private def genToken(): String = new BigInteger(130, random).toString(32)

  private def getGameBoard(gameid: Int): GameBoard = {
    val s: String = r.getGame(gameid)
    val g: GameBoard = readGameBoard(toJ(s))
    val m: GameMetaData = toMetaData(toJ(r.getMetaData(gameid)))
    g.metadata = m
    // replay game
    val p: Seq[String] = r.getPlayForGame(gameid)
    p.foreach(s => {
      val co: CommandValues = toParams(toJ(s));
      val comm: Command = action.constructCommand(co)
      playsingleCommand(g, comm)
    }
    )
    g
  }


  private def getBoard(token: String): (CurrentGame, GameBoard) = {
    val game: CurrentGame = r.getCurrentGame(token)
    (game, getGameBoard(game.gameid))
  }

  private def toC(com: Command): CommandValues = CommandValues(com.command, com.civ, com.p, com.j)

  def getData(what: Int, tokenorciv: String): String = {
    synchronized {
      what match {
        case LISTOFCIV => getListOfCiv()
        case REGISTEROWNER => registerOwnerPlay(tokenorciv, "GAME1.json")
        case REGISTEROWNERTWOGAME => registerOwnerPlay(tokenorciv, "GAME2.json")
        case GETBOARDGAME => getBoardForCiv(tokenorciv)
        case LISTOFGAMES => listOfGames
        case UNREGISTERTOKEN => {
          r.unregisterCurrentGame(tokenorciv)
          null
        }
        case LISTOFWAITINGGAMES => listOfWaitingGames
      }
    }
  }

  private def getListOfCiv(): String = {
    val tiles: Set[Civilization.T] = readListOfTiles.filter(_.tile.civhome).map(_.tile.civ).toSet
    Json.prettyPrint(writeListOfCiv(tiles contains _))
  }

  private def toCiv(civ: String): Civilization.T = Civilization.withName(civ)

  private def registerOwnerPlay(civ: String, game: String): String = {
    val l : List[Civilization.T] = civ.split(",").toList.map(toCiv(_))
    val g: GameBoard = genBoard(l, game)
    registerGame(g, l.head)
  }

  private def currentGame(civ: Civilization.T, gameid: Int): String = {
    val token: String = genToken()
    val cu: CurrentGame = CurrentGame(gameid, civ)
    r.registerCurrentGame(token, cu)
    token
  }

  def registerGame(g: GameBoard, civ: Civilization.T): String = {
    val gameS: String = writesGameBoard(g).toString()
    val gameid: Int = r.registerGame(gameS)
    val metadata: String = writeMetaData(g.metadata).toString()
    r.updateMetaData(gameid, metadata)
    // play
    g.play.commands.foreach(co => {
      val cc: CommandValues = toC(co)
      val s: String = writeCommandValues(cc).toString()
      r.addMoveToPlay(gameid, s)
    }
    )
    currentGame(civ, gameid)
  }

  def joinGame(gameid: Int, c: String): String = {
    val civ: Civilization.T = toCiv(c)
    currentGame(civ, gameid)
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

  private def touchGame(gameid: Int, g: GameBoard) = {
    g.metadata.accesstime = Calendar.getInstance().getTime.getTime
    r.updateMetaData(gameid, writeMetaData(g.metadata).toString())
  }

  def executeCommand(gb: (CurrentGame, GameBoard), com: CommandValues): Mess = {
    val co: Command = constructCommand(com)
    var mess: Mess = playCommand(gb._2, co, c => {
      val cv: CommandValues = toC(c)
      r.addMoveToPlay(gb._1.gameid, writeCommandValues(cv).toString())
    }
    )
    return mess
  }

  // for testing only
  def executeCommand(token: String, com: CommandValues): Mess = executeCommand(getBoard(token), com)

  def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String = {
    val gb = getBoard(token)
    val g: GameBoard = gb._2
    val civ: Civilization.T = gb._1.civ
    val command: Command.T = Command.withName(action)
    val coma: CommandValues = CommandValues(command, civ, if (row == -1) null else P(row, col), if (jsparam == null) null else toJ(jsparam))
    touchGame(gb._1.gameid, g)
    val m: Mess = executeCommand(gb, coma)
    if (m == null) null else m.toString
  }

  def itemizeCommand(token: String, action: String): String = {
    val command: Command.T = Command.withName(action)
    val g = getBoard(token)
    AllowedCommands.itemizeCommandS(g._2, g._1.civ, command)
  }

  private def listOfGames(): String = {
    // only the latest 50 sorted newest first
    val l: Seq[(Int, GameMetaData)] = r.getGames().map(s => (s._1, toMetaData(toJ(s._2)))).filter(_._2.okV).sortBy(_._2.accesstime)(Ordering.Long.reverse).take(50)
    val lg: Seq[(Int, GameBoard)] = l.map(p => (p._1, getGameBoard(p._1)))
    val ld: Seq[JsValue] = lg.map(p => {
      val g: GameBoard = p._2
      val civ: Seq[Civilization.T] = g.players.map(_.civ)
      val cu: CurrentPhase = currentPhase(g)
      GameData(p._1, civ, g.metadata.createtime, g.metadata.accesstime, cu.turnPhase, cu.roundno).as[JsValue]
    })
    Json.toJson(ld).toString()
  }

  def resumeGame(gameid: Int, c: String): String = {
    val civ: Civilization.T = toCiv(c)
    currentGame(civ, gameid)
  }

  def allPlayersReady(token: String): Boolean = {
    val game: CurrentGame = r.getCurrentGame(token)
    val w: Seq[Int] = WaitingGames.listofCurrentGames(r)
    w.find(_ == game.gameid).isDefined
  }

  /* for test only */
  def getBoardForToken(token: String): GameBoard = getBoard(token)._2

  private def listOfWaitingGames(): String = {
    var games: Seq[JsValue] = WaitingGames.findListOfWaitingGames(r)
    Json.toJson(games).toString()
  }

}

// to be visible from Java

object II {
  final val LISTOFCIV = I.LISTOFCIV
  final val REGISTEROWNER = I.REGISTEROWNER
  final val GETBOARDGAME = I.GETBOARDGAME
  final val LISTOFGAMES = I.LISTOFGAMES
  final val UNREGISTERTOKEN = I.UNREGISTERTOKEN
  final val LISTOFWAITINGGAMES = I.LISTOFWAITINGGAMES
  final val REGISTEROWNERTWOGAME = I.REGISTEROWNERTWOGAME

  def getData(what: Int, tokenorciv: String = null): String = I.getData(what, tokenorciv)

  def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String = I.executeCommand(token, action, row, col, jsparam)

  def itemizeCommand(token: String, action: String): String = I.itemizeCommand(token, action)

  def setR(r: RAccess) = I.setR(r)

  def resumeGame(gameid: Int, civ: String): String = I.resumeGame(gameid, civ)

  def joinGame(gameid: Int, civ: String): String = I.joinGame(gameid, civ)

  def allPlayersReady(token: String): Boolean = I.allPlayersReady(token)

}

