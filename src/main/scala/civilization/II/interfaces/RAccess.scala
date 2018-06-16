package civilization.II.interfaces

trait RAccess {

  def getConn : RConnection

  // current game cache
  def getCurrentGame(token: String): String

  def registerCurrentGame(token: String, content: String)

  def updateCurrentGame(token: String, content: String)

  def getCurrentGames() : Seq[String]

  def unregisterCurrentGame(token : String)

  def touchCurrentGame(token : String)

  def removeCurrentGames(remove : String => Boolean)

  // game
  def registerGame(value: String): Int

  def updateGame(id: Int, value: String)

  def getGame(id: Int): String

  // game actions

  def getPlayForGame(id: Int): Seq[String]

  def addMoveToPlay(id: Int, move: String)

  // game metadata
  def updateMetaData(id: Int, value: String)

  def getMetaData(id: Int): String

  def getGames(): Seq[(Int, String)]

}