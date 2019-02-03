package civilization.II.interfaces

import civilization.I

trait IC {

  final val LISTOFRES = I.LISTOFRES
  final val REGISTEROWNER = I.REGISTEROWNER
  final val GETBOARDGAME = I.GETBOARDGAME
  final val LISTOFGAMES = I.LISTOFGAMES
  final val UNREGISTERTOKEN = I.UNREGISTERTOKEN
  final val LISTOFWAITINGGAMES = I.LISTOFWAITINGGAMES
  final val REGISTEROWNERTWOGAME = I.REGISTEROWNERTWOGAME
  final val ITEMIZECOMMAND = I.ITEMIZECOMMAND
  final val GETJOURNAL = I.GETJOURNAL

  def getData(what: Int, tokenorciv: String = null, param : String = null): String

  def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String

  def itemizeCommand(token: String, action: String): String

  def setR(r: RAccess) : Unit

  def resumeGame(gameid: Int, civ: String): String

  def joinGame(gameid: Int, civ: String): String

  def allPlayersReady(token: String): Boolean

  def deleteGame(gameid : Int)

}
