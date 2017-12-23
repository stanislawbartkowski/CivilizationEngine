package civilization

import civilization.I.RAccess
import com.redis._

package object R {

  private final val CIVILIZATION = "civilization"
  private final val CURRENT = "current"
  private final val GAMES = "games"

  private final val SEQKEY = CIVILIZATION + "." + "seq"

  private final val PLAY = "play"

  private final val METADATA = "metadata"

  private final val ALLCURRENT = CIVILIZATION + "." + CURRENT + ".*"

  private final val rid = (CIVILIZATION + raw"\." + GAMES + raw"\." + raw"(\d*)." + METADATA).r

  private def extractGameId(s: String): Int = {
    //    "2004-01-20" match {
    //      case date(year, month, day) => s"$year was a good year for PLs."
    //    }
    s match {
      case rid(gameid) => gameid.toInt
    }
  }

  class R extends RAccess {

    //    private final val expireT: Int = 3600 * 24
    private final val expireT: Int = 3600

    private final val r: RedisClientPool = rr.get

    private def currentKey(token: String) = CIVILIZATION + "." + CURRENT + "." + token

    override def getCurrentGame(token: String): String = r.withClient(r => r.get(currentKey(token)).get)

    private def renewexpireT(token: String) = r.withClient(r => r.expire(currentKey(token), expireT))

    override def registerCurrentGame(token: String, content: String): Unit = {
      r.withClient(r => r.set(currentKey(token), content))
      renewexpireT(token)
    }

    override def updateCurrentGame(token: String, content: String): Unit = registerCurrentGame(token, content)

    private def getkeys(patt: String): List[String] = r.withClient(r => r.keys(patt)).get.filter(_.isDefined).map(_.get)

    override def getCurrentGames(): Seq[String] = {
      val patt: String = ALLCURRENT
      val keys: Seq[String] = getkeys(patt)
      r.withClient(r => keys.map(r.get(_).get))
    }

    override def unregisterCurrentGame(token: String): Unit = r.withClient(r => r.del(currentKey(token)))

    override def touchCurrentGame(token: String): Unit = renewexpireT(token)

    override def removeCurrentGames(remove: String => Boolean) = {
      val keys: Seq[String] = getkeys(ALLCURRENT)
      val rkeys: Seq[String] = keys.filter(k => {
        val co = r.withClient(r => r.get(k))
        remove(co.get)
      })
      rkeys.foreach(k => r.withClient(r => r.del(k)))
    }

    // ===================

    private def gameKey(id: Int): String = CIVILIZATION + "." + GAMES + "." + id

    override def registerGame(value: String): Int = {
      val id: Int = r.withClient(r => r.incr(SEQKEY).get.toInt)
      updateGame(id, value)
      id
    }

    override def updateGame(id: Int, value: String): Unit = r.withClient(r => r.set(gameKey(id), value))


    private def keyPlay(id: Int): String = gameKey(id) + "." + PLAY

    override def getPlayForGame(id: Int): Seq[String] = {
      val len: Int = r.withClient(r => r.llen(keyPlay(id)).get.toInt)
      for (i <- 0 until len) yield r.withClient(r => r.lindex(keyPlay(id), i.toInt).get)
    }

    override def addMoveToPlay(id: Int, move: String): Unit = r.withClient(r => r.rpush(keyPlay(id), move))

    override def getGame(id: Int): String = r.withClient(r => r.get(gameKey(id)).get)

    def keyMetaData(id: Int) = gameKey(id) + "." + METADATA

    override def updateMetaData(id: Int, value: String): Unit = r.withClient(r => r.set(keyMetaData(id), value))

    override def getMetaData(id: Int): String = r.withClient(r => r.get(keyMetaData(id)).get)

    override def getGames(): Seq[(Int, String)] = {
      val keyspattern: String = CIVILIZATION + "." + GAMES + ".*." + METADATA
      val keys: List[String] = getkeys(keyspattern)
      keys.map(extractGameId(_)).map(g => (g, getMetaData(g)))
    }

  }

  private var rr: Option[RedisClientPool] = None

  // singleton RedisClient to keep single connection to Redis
  // assuming that host and port does not change
  // in Heroku, if JVM changes initizalize new Redis connection
  def setConnection(host: String, port: Int, pdatabase: Int): Unit = {
    if (rr.isEmpty) rr = Some(new RedisClientPool(host, port, database = pdatabase))
  }

  def setConnection(url: String): Unit = {
    val u: java.net.URI = new java.net.URI(url)
    // copied and pasted
    val psecret: Option[Any] = Option(u.getUserInfo)
      .flatMap(_.split(':') match {
        case Array(_, password, _*) ⇒ Some(password)
        case _ ⇒ None
      })
    if (rr.isEmpty) rr = Some(new RedisClientPool(u.getHost, u.getPort, secret = psecret))
  }

  // multiply R instances but accessing single connection all the time
  def R: RAccess = new R()

}


// to be visible from Java
object RR {
  def setConnection(host: String, port: Int, database: Int = 0) = R.setConnection(host, port, database)

  def setConnection(url: String): Unit = R.setConnection(url)

  def RA: RAccess = R.R
}
