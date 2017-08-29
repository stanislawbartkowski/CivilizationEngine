package civilization

import civilization.I.RAccess
import com.redis._

package object R {

  class R extends RAccess {

    private final def CIVILIZATION = "civilization"
    private final def CURRENT = "current"
    private final def GAMES = "games"
    private final def SEQKEY = CIVILIZATION + "." + "seq"
    private final def PLAY="play"

    private def currentKey(token : String) = CIVILIZATION + "."  + token

    override def getCurrentGame(token: String): String = r.get(currentKey(token)).get

    override def registerCurrentGame(token: String, content : String): Unit = r.set(currentKey(token),content)

    override def updateCurrentGame(token: String, content: String): Unit = registerCurrentGame(token,content)

    private def gameKey(id : Int) : String = CIVILIZATION + "." + GAMES + "." + id

    override def registerGame(value: String): Int = {
      val id : Int = r.incr(SEQKEY).get.toInt
      updateGame(id, value)
      id
    }

    override def updateGame(id: Int, value: String): Unit = r.set(gameKey(id),value)

    private def keyPlay(id : Int) : String = gameKey(id) + "." + PLAY

    override def getPlayForGame(id: Int): Seq[String] = {
      val len : Int = r.llen(keyPlay(id)).get.toInt
      for ( i <- 0 until len) yield r.lindex(keyPlay(id),i.toInt).get
    }

    override def addMoveToPlay(id: Int, move: String): Unit = r.rpush(keyPlay(id),move)

    override def getGame(id: Int): String = r.get(gameKey(id)).get
  }

  private var r : RedisClient = null

  def setConnection(host : String, port : Int): Unit = r = new RedisClient(host,port)

  def R : RAccess = new R()

}
