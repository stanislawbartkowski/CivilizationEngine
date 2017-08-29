package civilization.I

trait RAccess {

  def getCurrentGame(token : String) : String

  def registerCurrentGame(token : String, content : String)

  def updateCurrentGame(token : String, content : String)

  def registerGame(value : String) : Int

  def updateGame(id : Int, value: String)

  def getGame(id : Int) : String

  def getPlayForGame(id : Int) : Seq[String]

  def addMoveToPlay(id : Int, move : String)

}
