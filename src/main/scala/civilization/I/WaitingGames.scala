package civilization.I


import civilization.io.fromjson._
import civilization.objects._

object WaitingGames {

  def findListOfWaitingGames(r: RAccess): Seq[(Int, Seq[Civilization.T])] = {
    val current: Seq[CurrentGame] = r.getCurrentGames()
    // group by current games
    val games: Map[Int, Seq[CurrentGame]] = current.groupBy(_.gameid)
    // group games by number of players
    val nplayes: Map[Int, Int] = r.getGames().map(g => (g._1, toGameBoard(toJ(r.getGame(g._1))))).map(gg => gg._1 -> gg._2.players.length).toMap
    // filter out all underplayed
    games.filter(g => nplayes(g._1) > g._2.length).map(c => (c._1, games(c._1).map(p => p.civ))).toSeq
  }

  def listofCurrentGames(r: RAccess): Seq[Int] = {
    val current: Seq[CurrentGame] = r.getCurrentGames()
    val currentSet: Set[Int] = current.map(_.gameid).toSet
    val waitingSet: Set[Int] = findListOfWaitingGames(r).map(_._1).toSet
    (currentSet -- waitingSet).toSeq
  }

}
