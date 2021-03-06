package discoverytest

import java.math.BigInteger
import java.security.SecureRandom

import civilization.RR
import civilization.II.factory._

object Main5 {

  private val random = new SecureRandom()

  private def genToken(): String = new BigInteger(130, random).toString(32)

  def main1(): Unit = {
    val r = Factory.getR
    val token: String = genToken()
    r.registerCurrentGame(token, "aaaa")
    println(r.getCurrentGame(token))
    r.updateCurrentGame(token, "cccccc")
    println(r.getCurrentGame(token))

    val id = r.registerGame("game")
    println(id)
    println(r.getGame(id))
    r.updateGame(id, "next game")
    println(r.getGame(id))
    println(r.getPlayForGame(id))
    r.addMoveToPlay(id, "move1")
    r.addMoveToPlay(id, "move2")
    r.addMoveToPlay(id, "move3")
    r.addMoveToPlay(id, "move4")
    println(r.getPlayForGame(id))
    r.addMoveToPlay(id, "move5")
    println(r.getPlayForGame(id))
  }

  def main2() = {
    val r = Factory.getR
    val p: Seq[(Int, String)] = r.getGames()
    p.foreach(println)
  }

  def main(args: Array[String]): Unit = {

    Factory.getR.getConn.setConnection("localhost", 6379)
    //    main1
    main2

  }

}
