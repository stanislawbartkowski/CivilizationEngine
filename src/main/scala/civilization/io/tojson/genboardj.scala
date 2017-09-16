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
  case class MapSquareJ(revealed: Boolean, t: Terrain.T, trade: Int, production: Int, resource: Resource.T, capForCiv: Civilization.T,
                        civ: Civilization.T, city: City.T, defence: Int, numberofArmies: Int, numberofScouts: Int, tile: String, hv: HutVillage.T)

  case class PlayerDeckJ(civ: Civilization.T, numberofTrade: Int, commands: Seq[Command.T], limits: PlayerLimits)

  case class Game(active: Civilization.T, roundno: Int, phase: TurnPhase.T)

  case class BoardGameJ(g: Game, map: Array[Array[MapSquareJ]], you: PlayerDeckJ, others: Seq[PlayerDeckJ])

  private def contructSquareJ(b: GameBoard, ss: MapSquareP): MapSquareJ = {
    val t: Terrain.T = if (ss.revealed) ss.terrain else null;
    val trade: Int = if (ss.revealed) ss.numberOfTrade else -1;
    val production: Int = if (ss.revealed) (if (ss.s.city == null) ss.numberOfProduction else getProductionForCity(b, ss.p)) else -1;
    val resource: Resource.T = if (ss.revealed) ss.resource else null
    val cap: Civilization.T = ss.suggestedCapitalForCiv
    var civ: Civilization.T = null
    var defence: Int = 0
    var city: City.T = null
    if (ss.s.city != null) {
      civ = ss.s.city.civ
      city = ss.s.city.citytype
      defence = ss.s.city.defenceStrength()
    }
    var numberofArmies: Int = -1
    var numberofScouts: Int = -1
    if (!ss.s.figures.empty) {
      civ = ss.s.figures.civ
      numberofArmies = ss.s.figures.numberofArmies
      numberofScouts = ss.s.figures.numberofScouts
    }

    MapSquareJ(ss.revealed, t, trade, production, resource, cap, civ, city, defence, numberofArmies, numberofScouts, ss.t.tname,
      if (ss.s.hv != null) ss.s.hv.hv else null)
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
    val others: Seq[PlayerDeckJ] = g.players.filter(_.civ != civ).map(c => genPlayerDeckJ(g, c.civ))
    BoardGameJ(genGame(g, civ), map, genPlayerDeckJ(g, civ), others)
  }

  private def mapSquareJ(m: MapSquareJ): JsValue = {
    Json.obj("revealed" -> m.revealed, "terrain" -> Option(m.t), S.trade -> m.trade, "production" -> m.production, "resource" -> Option(m.resource),
      "capciv" -> Option(m.capForCiv), S.civ -> Option(m.civ), S.city -> Option(m.city), "defence" -> m.defence, S.numberofArmies -> m.numberofArmies, S.numberofScouts -> m.numberofScouts,
      "tile" -> m.tile, S.hutvillage -> Option(m.hv)
    )
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
    val o: Seq[JsValue] = b.others.map(genPlayerDeckJson(_))

    JsObject(Seq("board" -> JsObject(Seq(
      "map" -> JsArray(rows),
      "game" -> gameToJ(b.g),
      "you" -> genPlayerDeckJson(b.you),
      "others" -> JsArray(o)
    ))))
  }


}
