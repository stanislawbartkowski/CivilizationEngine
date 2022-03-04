package civilization.I


import civilization.II.interfaces.RAccess
import civilization.io.fromjson._
import civilization.objects._
import play.api.libs.json.{JsValue, Json}
import scala.language.postfixOps

object WaitingGames {

  case class WaitingGames(val gameid: Int, val createTime: Long, val registeredplayers: Seq[Civilization.T], val waiting: Seq[Civilization.T])

  implicit def convert(g: WaitingGames): JsValue = {
    val j: JsValue = Json.obj(
      S.gameid -> g.gameid,
      S.createtime -> g.createTime,
      "players" -> g.registeredplayers,
      "waiting" -> g.waiting
    )
    j
  }

  implicit def convert(g: Seq[WaitingGames]): Seq[JsValue] = g.map(convert(_))

  def findListOfWaitingGames(r: RAccess): Seq[WaitingGames] = {
    val current: Seq[CurrentGame] = r.getCurrentGames()
    // group by current games
    val games: Map[Int, Seq[CurrentGame]] = current.groupBy(_.gameid)
    // group games by list of players
    // filter out out of version games
    val nplayes: Map[Int, Seq[Civilization.T]] = r.getGames().filter(m => toMetaData(toJ(m._2)).okV).
      map(g => (g._1, toGameBoard(toJ(r.getGame(g._1).get)))).map(gg => gg._1 -> gg._2.players.map(_.civ)).toMap
    // filter out all underplayed
    games.filter(
      // all underplayed game
      g => nplayes(g._1).length > g._2.length).
      //
      map(c => WaitingGames(
      c._1,
      games(c._1).map(p => p.createtime) min,
      games(c._1).map(p => p.civ),
      nplayes(c._1) diff games(c._1).map(p => p.civ))).
      toSeq.sortBy(_.createTime)(Ordering.Long.reverse)
  }

  def listofCurrentGames(r: RAccess): Seq[Int] = {
    val current: Seq[CurrentGame] = r.getCurrentGames()
    val currentSet: Set[Int] = current.map(_.gameid).toSet
    val waitingSet: Set[Int] = findListOfWaitingGames(r).map(_.gameid).toSet
    (currentSet -- waitingSet).toSeq
  }

}
