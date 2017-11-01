package civilization.I

import civilization.I

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

  def setR(r: I.RAccess) = I.setR(r)

  def resumeGame(gameid: Int, civ: String): String = I.resumeGame(gameid, civ)

  def joinGame(gameid: Int, civ: String): String = I.joinGame(gameid, civ)

  def allPlayersReady(token: String): Boolean = I.allPlayersReady(token)

}
