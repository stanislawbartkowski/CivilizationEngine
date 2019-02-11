package civilization.I

import civilization.I
import civilization.II.interfaces.{IC, RAccess}

class II extends IC {

  override def getData(what: Int, tokenorciv: String = null, param: String = null): String = I.getData(what, tokenorciv, param)

  override def executeCommand(token: String, action: String, row: Int, col: Int, jsparam: String): String = I.executeCommand(token, action, row, col, jsparam)

  override def itemizeCommand(token: String, action: String): String = getData(ITEMIZECOMMAND, token, action)

  override def setR(r: RAccess) = I.setR(r)

  override def resumeGame(gameid: Int, civ: String): String = I.resumeGame(gameid, civ)

  override def joinGame(gameid: Int, civ: String): String = I.joinGame(gameid, civ)

  override def allPlayersReady(token: String): Boolean = I.allPlayersReady(token)

  override def deleteGame(gameid: Int) = I.deleteGame(gameid)

  override def readPlayerGameS(board: String, civs: String): String = I.readPlayerGameS(board, civs)

  override def downloadGame(gameid: Int): String = I.downloadGame(gameid)

}
