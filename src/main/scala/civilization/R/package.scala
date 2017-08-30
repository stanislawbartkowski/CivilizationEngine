package civilization

import civilization.I.RAccess
import com.redis._

package object R {

  class R extends RAccess {

    private final val r: RedisClient = rr.get

    private final def CIVILIZATION = "civilization"

    private final def CURRENT = "current"

    private final def GAMES = "games"

    private final def SEQKEY = CIVILIZATION + "." + "seq"

    private final def PLAY = "play"

    private def currentKey(token: String) = CIVILIZATION + "." + token

    override def getCurrentGame(token: String): String = r.get(currentKey(token)).get

    override def registerCurrentGame(token: String, content: String): Unit = r.set(currentKey(token), content)

    override def updateCurrentGame(token: String, content: String): Unit = registerCurrentGame(token, content)

    private def gameKey(id: Int): String = CIVILIZATION + "." + GAMES + "." + id

    override def registerGame(value: String): Int = {
      val id: Int = r.incr(SEQKEY).get.toInt
      updateGame(id, value)
      id
    }

    override def updateGame(id: Int, value: String): Unit = r.set(gameKey(id), value)

    private def keyPlay(id: Int): String = gameKey(id) + "." + PLAY

    override def getPlayForGame(id: Int): Seq[String] = {
      val len: Int = r.llen(keyPlay(id)).get.toInt
      for (i <- 0 until len) yield r.lindex(keyPlay(id), i.toInt).get
    }

    override def addMoveToPlay(id: Int, move: String): Unit = r.rpush(keyPlay(id), move)

    override def getGame(id: Int): String = r.get(gameKey(id)).get
  }

  private var rr: Option[RedisClient] = None

  // singleton RedisClient to keep single connection to Redis
  // assuming that host and port does not change
  // in Heroku, if JVM changes initizalize new Redis connection
  def setConnection(host: String, port: Int): Unit = {
    if (rr.isEmpty) rr = Some(new RedisClient(host, port))
  }

  // multiply R instances but accessing single connection all the time
  def R: RAccess = new R()

}
