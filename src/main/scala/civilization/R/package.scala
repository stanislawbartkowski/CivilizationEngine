package civilization

import civilization.I.RAccess
import com.redis._

package object R {

  private final val CIVILIZATION = "civilization"
  private final val CURRENT = "current"
  private final val GAMES = "games"
  private final def SEQKEY = CIVILIZATION + "." + "seq"
  private final def PLAY = "play"
  private final def METADATA = "metadata"
  private final val reg = raw"(\d{4})-(\d{2})-(\d{2})".r
  private final val rid = (CIVILIZATION + raw"\." + GAMES + raw"\." + raw"(\d*)." + METADATA).r

  private def extractGameId(s : String) : Int = {
//    "2004-01-20" match {
//      case date(year, month, day) => s"$year was a good year for PLs."
//    }
      s match {
        case rid(gameid) => gameid.toInt
      }
  }

  class R extends RAccess {

    private final val expireT: Int = 3600 * 24

    private final val r: RedisClient = rr.get

    private def currentKey(token: String) = CIVILIZATION + "." + token

    override def getCurrentGame(token: String): String = r.get(currentKey(token)).get

    override def registerCurrentGame(token: String, content: String): Unit = {
      r.set(currentKey(token), content)
      // remove after one day
      r.expire(currentKey(token), expireT)
    }

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

    def keyMetaData(id: Int) = gameKey(id) + "." + METADATA

    override def updateMetaData(id: Int, value: String): Unit = r.set(keyMetaData(id), value)

    override def getMetaData(id: Int): String = r.get(keyMetaData(id)).get

    override def getGames(): Seq[(Int, String)] = {
      val keyspattern : String = CIVILIZATION + "." + GAMES + ".*." + METADATA
      val keys: List[String] = r.keys(keyspattern).get.filter(_.isDefined).map(_.get)
      keys.map(extractGameId(_)).map(g => (g,getMetaData(g)))
    }
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
