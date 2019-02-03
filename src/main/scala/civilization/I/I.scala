package civilization

import java.math.BigInteger
import java.security.SecureRandom
import java.util.Calendar

import civilization.II.interfaces.{RAccess, ICache}
import civilization.action._
import civilization.gameboard.{GameBoard, GameMetaData}
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.readdir.GenBoard.genBoard
import civilization.io.readdir._
import civilization.io.tojson._
import civilization.message._
import civilization.objects._
import play.api.libs.json._
import civilization.io.readdir.GameResources
import civilization.II.factory.{Factory}

package object I extends ImplicitMiximFromJson {

  private var r: RAccess = _

  def setR(r: RAccess) = {
    this.r = r
  }

  final val LISTOFRES: Int = 0
  final val REGISTEROWNER: Int = 1
  final val GETBOARDGAME: Int = 2
  final val LISTOFGAMES: Int = 3
  final val UNREGISTERTOKEN: Int = 4
  final val LISTOFWAITINGGAMES: Int = 5
  final val REGISTEROWNERTWOGAME: Int = 6
  final val ITEMIZECOMMAND: Int = 7
  final val GETJOURNAL: Int = 8

  private val random = new SecureRandom()

  private def genToken(): String = new BigInteger(130, random).toString(32)

  private def getGameBoard(gameid: Int): GameBoard = {
    val s: String = r.getGame(gameid)
    val g: GameBoard = readGameBoard(toJ(s))
    val m: GameMetaData = toMetaData(toJ(r.getMetaData(gameid)))
    g.metadata = m
    // replay game
    val p: Seq[String] = r.getPlayForGame(gameid)
    // read journal as list of JSon String
    val j: Seq[String] = r.getJournalForGame(gameid)
    // transform to JournalElem
    j.foreach(s => {
      val j: JsValue = toJ(s); g.addJ(toJournalElem(j))
    })
    // only last command may have suspended status
    var counter: Int = 0 // to identify the last command
    p.foreach(s => {
      val co: CommandValues = toJ(s)
      val comm: Command = action.constructCommand(co)
      if (comm.isSuspended && counter != p.length - 1)
      // break stuff
        throw FatalError(Mess(M.ONLYLASTCOMMANDCANHAVESUPENDEDSTATUS, (counter, p.length - 1, comm)))
      comm.setReplay
      playsingleCommand(g, comm)
      counter = counter + 1
    }
    )
    g
  }


  private def getBoard(token: String): (CurrentGame, GameBoard) = {
    val game: CurrentGame = r.getCurrentGame(token)

    r.touchCurrentGame(token)
    (game, Factory.getIC.getT(game.gameid, getGameBoard))
  }

  private def getBoardForCiv(token: String): String = {
    val game: CurrentGame = r.getCurrentGame(token)
    // game metadata
    val m: GameMetaData = toMetaData(toJ(r.getMetaData(game.gameid)))
    // nothing has changed
    if (game.boardtimemili.isDefined && game.boardtimemili.get == m.modiftimemili) return ""
    // update board current time in CurrentGame
    game.boardtimemili = Some(m.modiftimemili)
    r.updateCurrentGame(token, game)
    //  val g : GameBoard = getGameBoard(game.gameid)
    val g: GameBoard = Factory.getIC.getT(game.gameid, getGameBoard)
    Json.prettyPrint(genboardj.genBoardGameJson(g, g.playerDeck(game.civ)))
  }

  private def getJournal(token: String): String = {
    val g = getBoard(token)
    val j: JsValue = writeJ(g._1.civ, g._2.journal)
    Json.prettyPrint(j)
  }

  private def toS(s : (String, Int)) : String = s._1 + "," + s._2

  def getData(what: Int, tokenorciv: String, param: String): String = {
    synchronized {
      what match {
        case REGISTEROWNER => toS(registerOwnerPlay(tokenorciv, "GAME1.json"))
        case REGISTEROWNERTWOGAME => toS(registerOwnerPlay(tokenorciv, "GAME2.json"))
        case GETBOARDGAME => getBoardForCiv(tokenorciv)
        case LISTOFGAMES => listOfGames
        case UNREGISTERTOKEN => {
          r.unregisterCurrentGame(tokenorciv)
          null
        }
        case LISTOFWAITINGGAMES => listOfWaitingGames
        case ITEMIZECOMMAND => itemizeCommand(tokenorciv, param)
        case LISTOFRES => {
          val j: JsValue = GameResources.instance()
          Json.prettyPrint(j)
        }
        case GETJOURNAL => getJournal(tokenorciv)
      }
    }
  }

  private def toCiv(civ: String): Civilization.T = Civilization.withName(civ)

  private def registerOwnerPlay(civ: String, game: String): (String,Int) = {
    val l: List[Civilization.T] = civ.split(",").toList.map(toCiv(_))
    val g: GameBoard = genBoard(l, game)
    registerGame(g, l.head)
  }

  private def currentGame(civ: Civilization.T, gameid: Int): String = {
    val token: String = genToken()
    val cu: CurrentGame = CurrentGame(gameid, civ)
    r.registerCurrentGame(token, cu)
    token
  }

  def registerGame(g: GameBoard, civ: Civilization.T): (String,Int) = {
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
    updateJournal(gameid, g)
    (currentGame(civ, gameid),gameid)
  }

  def joinGame(gameid: Int, c: String): String = {
    val civ: Civilization.T = toCiv(c)
    currentGame(civ, gameid)
  }

  private def getBoardName(token: String): String = {
    val g = getBoard(token)
    Json.prettyPrint(writesGameBoard(g._2))
  }

  private def touchGame(gameid: Int, g: GameBoard) = {
    g.metadata.accesstime = Calendar.getInstance().getTime.getTime
    r.updateMetaData(gameid, writeMetaData(g.metadata).toString())
  }

  private def updateJournal(gameid : Int, b: GameBoard) = {
    // current stored journal size
    val nj: Int = r.numberOfJournalE(gameid)
    // number of new elements
    val addN: Int = b.journal.length - nj
    // get last addN elements
    assert(addN >= 0)
    b.journal.reverse.take(addN).reverse.foreach(j => {
      val jval: JsValue = writeJournalElem(j)
      r.addJournalEntry(gameid, jval.toString())
    }
    )
  }

  private def executeCommand(gb: (CurrentGame, GameBoard), com: CommandValues): Mess = {
    val co: Command = constructCommand(com)
    var mess: Mess = playCommand(gb._2, co, c => {
      val cv: CommandValues = toC(c)
      r.addMoveToPlay(gb._1.gameid, writeCommandValues(cv).toString())
      updateJournal(gb._1.gameid,gb._2)
    },
      counter => {
        val cv: CommandValues = toC(gb._2.play.commands(counter))
        r.replaceMoveToPlay(gb._1.gameid, counter, writeCommandValues(cv).toString())
        updateJournal(gb._1.gameid, gb._2)
      }
    )
    // modif metadata with current timestamp
    gb._2.metadata.modiftimestamp()
    touchGame(gb._1.gameid, gb._2)
    return mess
  }

  def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String = {
    val gb = getBoard(token)
    val g: GameBoard = gb._2
    val civ: Civilization.T = gb._1.civ
    val command: Command.T = Command.withName(action)
    val coma: CommandValues = CommandValues(command, civ, if (row == -1) null else P(row, col), CommandStatus.No, if (jsparam == null) null else toJ(jsparam))

    touchGame(gb._1.gameid, g)
    val m: Mess = executeCommand(gb, coma)
    if (m == null) null else m.toString
  }

  private def itemizeCommand(token: String, action: String): String = {
    val command: Command.T = Command.withName(action)
    val g = getBoard(token)
    AllowedCommands.itemizeCommandS(g._2, g._2.playerDeck(g._1.civ), command)
  }

  private def listOfGames(): String = {
    // only the latest 50 sorted newest first
    val l: Seq[(Int, GameMetaData)] = r.getGames().map(s => (s._1, toMetaData(toJ(s._2)))).filter(_._2.okV).sortBy(_._2.accesstime)(Ordering.Long.reverse).take(50)
    val lg: Seq[(Int, GameBoard)] = l.map(p => (p._1, getGameBoard(p._1)))
    val ld: Seq[JsValue] = lg.map(p => {
      val g: GameBoard = p._2
      val civ: Seq[Civilization.T] = g.players.map(_.civ)
      val cu: CurrentPhase = currentPhase(g)
      GameData(p._1, civ, g.metadata.createtime, g.metadata.accesstime, cu.turnPhase, cu.roundno, g.endofgame).as[JsValue]
    })
    Json.toJson(ld).toString()
  }

  private def removeexistinggames(gameid: Int): Unit = {
    r.removeCurrentGames(co => {
      val c: CurrentGame = co
      c.gameid == gameid
    })
  }

  def resumeGame(gameid: Int, c: String): String = {
    removeexistinggames(gameid)
    val civ: Civilization.T = toCiv(c)
    currentGame(civ, gameid)
  }

  def deleteGame(gameid : Int) = r.deleteGame(gameid)

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
