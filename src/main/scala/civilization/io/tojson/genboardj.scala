package civilization.io.tojson

import civilization.gameboard._
import civilization.helper._
import civilization.objects._
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import civilization.helper.AllowedCommands.allowedCommands

object genboardj {

  // production:
  // if empty square : number of original production
  // if city: number of city production
  case class MapSquareJ(val revealed: Boolean, val t: Terrain.T, val trade: Int, val production: Int, val resource: Resource.T, val capForCiv: Civilization.T,
                        val civ: Civilization.T, val city: City.T, val numberofArmies: Int, val numberofScouts: Int)

  case class PlayerDeckJ(val civ: Civilization.T, val numberofTrade: Int, val commands: Seq[Command.T], val limits: PlayerLimits)

  case class Game(val active: Civilization.T, val roundno: Int, val phase: TurnPhase.T)

  case class BoardGameJ(val g: Game, val map: Array[Array[MapSquareJ]], val you: PlayerDeckJ)

  private def contructSquareJ(b: GameBoard, ss: MapSquareP): MapSquareJ = {
    val t: Terrain.T = if (ss.revealed) ss.terrain else null;
    val trade: Int = if (ss.revealed) ss.numberOfTrade else -1;
    val production: Int = if (ss.revealed) (if (ss.s.city == null) ss.numberOfProduction else getProductionForCity(b, ss.p)) else -1;
    val resource: Resource.T = if (ss.revealed) ss.resource else null
    val cap: Civilization.T = ss.suggestedCapitalForCiv
    var civ: Civilization.T = null
    var city: City.T = null
    if (ss.s.city != null) {
      civ = ss.s.city.civ
      city = ss.s.city.citytype
    }
    var numberofArmies: Int = -1
    var numberofScouts: Int = -1
    if (!ss.s.figures.empty) {
      civ = ss.s.figures.civ
      numberofArmies = ss.s.figures.numberofArmies
      numberofScouts = ss.s.figures.numberofScouts
    }

    MapSquareJ(ss.revealed, t, trade, production, resource, cap, civ, city, numberofArmies, numberofScouts)
  }

  private def genGame(g: GameBoard, civrequesting: Civilization.T): Game = {

    val cu: CurrentPhase = currentPhase(g)
    // TODO: active civilization, later
    var civ: Civilization.T = cu.notcompleted.head
    cu.turnPhase match {
        // for StartOfTurn and Trade all are active
      case TurnPhase.StartOfTurn | TurnPhase.Trade => {
        if (cu.notcompleted.contains(civrequesting)) civ = civrequesting
      }
      case _ =>
    }
    Game(civ, cu.roundno, cu.turnPhase)
  }

  private def genPlayerDeckJ(g: GameBoard, civ: Civilization.T): PlayerDeckJ = PlayerDeckJ(civ, numberofTrade(g, civ), allowedCommands(g, civ), getLimits(g, civ))

  private def commandToArray(l: Seq[Command.T]): JsArray = {
    JsArray(l.map(c => Json.obj(S.command -> c)).foldLeft(List[JsObject]())(_ :+ _))
  }

  private def genPlayerDeckJson(p: PlayerDeckJ): JsValue = Json.obj(
    S.civ -> p.civ,
    "trade" -> p.numberofTrade,
    "commands" -> commandToArray(p.commands),
    "citylimit" -> p.limits.citieslimit,
    "armieslimit" -> p.limits.armieslimit,
    "scoutslimit" -> p.limits.scoutslimit
  )

  private def genBoardGameJ(g: GameBoard, civ: Civilization.T): BoardGameJ = {
    val p: Seq[MapSquareP] = allSquares(g)
    val maxrow: Int = p.map(_.p.row).max
    val maxcol: Int = p.map(_.p.col).max
    val map: Array[Array[MapSquareJ]] = Array.ofDim(maxrow + 1, maxcol + 1)
    // TODO: initialization to null maybe unnecessary, doublecheck
    for (i <- 0 to maxrow; j <- 0 to maxcol) map(i)(j) = null
    p.foreach(ss => map(ss.p.row)(ss.p.col) = contructSquareJ(g, ss))
    BoardGameJ(genGame(g, civ), map, genPlayerDeckJ(g, civ))
  }

  private def mapSquareJ(m: MapSquareJ): JsValue = {
    Json.obj("revealed" -> m.revealed, "terrain" -> Option(m.t), S.trade -> m.trade, "production" -> m.production, "resource" -> Option(m.resource),
      "capciv" -> Option(m.capForCiv), S.civ -> Option(m.civ), S.city -> Option(m.city), S.numberofArmies -> m.numberofArmies, S.numberofScouts -> m.numberofScouts)
  }

  private def gameToJ(g: Game): JsValue = {
    Json.obj("roundno" -> g.roundno, "phase" -> g.phase, "active" -> g.active)
  }

  def genBoardGameJson(g: GameBoard, civ: Civilization.T): JsValue = {
    val b: BoardGameJ = genBoardGameJ(g, civ)
    var rows: Seq[JsValue] = Seq()

    for (row <- 0 until b.map.length) {
      var cols: Seq[JsValue] = Nil
      for (col <- 0 until b.map(row).length) {
        cols = cols :+ mapSquareJ(b.map(row)(col))
      }
      rows = rows :+ JsArray(cols)
    }
    JsObject(Seq("board" -> JsObject(Seq(
      "map" -> JsArray(rows),
      "game" -> gameToJ(b.g),
      "you" -> genPlayerDeckJson(b.you)
    ))))
  }


}
