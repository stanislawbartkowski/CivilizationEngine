package civilization.helper.util

import civilization.II.interfaces.{ICache}
import civilization.gameboard.{GameBoard}
import java.util.{Calendar,Date}

class Cache extends ICache {

  private def getCurrent : Date = Calendar.getInstance.getTime

  private case class Cached(val g : GameBoard) {
    var timestamp : Date = getCurrent
    def touch = timestamp = getCurrent
  }

  // initialize as empty
  private val cache : scala.collection.mutable.Map[Int,Cached] = scala.collection.mutable.Map()

  override def getT(id: Int, retrieve: Int => GameBoard): GameBoard = {
    var c: Option[Cached] = cache.get(id)
    if (c.isEmpty || !isCached) {
      c = Some(Cached(retrieve(id)))
      cache.put(id, c.get)
    }
    c.get.touch
    c.get.g
  }

  override def isCached: Boolean = true

}
