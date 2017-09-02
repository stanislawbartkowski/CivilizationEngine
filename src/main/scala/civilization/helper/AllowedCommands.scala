package civilization.helper

import civilization.gameboard.{Figures, GameBoard}
import civilization.io.tojson._
import civilization.message._
import civilization.objects._
import play.api.libs.json.{JsArray, JsValue, Json}

object AllowedCommands {

  // City Management, BUTSCOUT, BUYARMY
  private def allowedActionForCityManagement(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    var cu: Seq[Command.T] = Nil
    if (!itemizeForSetBuyFigures(b, civ, Command.BUYSCOUT).isEmpty) cu = cu :+ Command.BUYSCOUT
    if (!itemizeForSetBuyFigures(b, civ, Command.BUYARMY).isEmpty) cu = cu :+ Command.BUYARMY
    cu
  }

  def itemizeForSetBuyFigures(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[(P, P)] = {
    val li: Seq[MapSquareP] = citiesForCivilization(b, civ)
    val fi: Figure.T = if (com == Command.SETARMY || com == Command.BUYARMY) Figure.Army else Figure.Scout

    var alist: Seq[(P, P)] = li.flatMap(s => pointsAround(b, s.p).map(p => (s.p, p)).filter(po => isSquareForFigure(b, civ, fi, po._2).isEmpty))
    if (com == Command.BUYSCOUT || com == Command.BUYARMY) {
      // remove all cities already used in CityManagement
      val cities: Set[P] = CityAvailableForAction(b, civ).toSet
      alist = alist.filter(ci => cities contains ci._1).filter(city => (getProductionForCity(b, city._1) >= ObjectCost.getCost(fi)))
    }
    alist
  }

  // Figure Movement

  private def allowedActionForMovement(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    val itemi: Seq[(Figures, P)] = itemizeforStartOfMove(b: GameBoard, civ: Civilization.T)
    if (itemi.isEmpty) Nil else Seq(Command.STARTMOVE)
  }

  def itemizeforStartOfMove(b: GameBoard, civ: Civilization.T): Seq[(Figures, P)] = {
    val lastp: Seq[(Figures, P, P)] = civLastMoves(b, civ).map(o => (o.f.toFigures, o.begstop._1.p.get, o.begstop._2.p.get))
    //TODO: can be done better,2 traversals, use mutable map and fold
    //    val startmap : Map[P,Figures] = lastp.map(t => t._3 -> Figures(0,0)) toMap
    //    val lastm: Map[P, Figures] = lastp.map(t => t._3 -> t._1).toMap
    // current points on board belonging to civilization
    //    val current: Seq[(Figures, P)] = allSquares(b).filter(p => p.s.figures.civOccupying.isDefined && p.s.figures.civOccupying.get == civ).map(m => (m.s.figures.toFigures, m.p))
    // sum all figures finishing at given point
    val lastm: Map[P, Figures] = lastp.groupBy(_._3).map(e => e._1 -> e._2.foldLeft[Figures](Figures(0, 0))((f, p) => {
      f + p._1; f
    }))
    val current: Seq[(Figures, P)] = getFigures(b, civ).map(m => (m.s.figures.toFigures, m.p))
    // remove all figures which has moved already
    current.foreach(f => {
      val l: Option[Figures] = lastm.get(f._2)
      if (l.isDefined) f._1 - l.get
    })
    // throw away all empty
    current.filter(!_._1.empty)
  }

  // reveal: figure point, tile point, orientation
  case class PossibleMove(var p: P, val reveal: Seq[(P, Orientation.T)], val move: Seq[P])

  // return
  //  None : no move
  //  Some(null) : ENDMOVE
  //  Some(PossibleMove) : MOVE
  // TODO: I'm not happy with that design
  def itemizeForMove(b: GameBoard, civ: Civilization.T): Option[PossibleMove] = {
    val ll: Seq[PlayerMove] = civLastMoves(b, civ)
    if (ll.isEmpty || ll.last.isFinished) return None
    val lim: PlayerLimits = getLimits(b, civ)
    // only ENDMOVE possible
    val last: PlayerMove = ll.last
    if (last.len == lim.travelSpeed) return Some(null)
    val p: P = last.lastp
    // only direct point, 4 directions
    val around: Seq[MapSquareP] = squaresAround(b, p)
    val points: Seq[(MapSquareP, Option[Mess])] = around.map(p => (p, MoveAction.figureMovePointCheck(b, civ, last, p.p, last.len + 1 == lim.travelSpeed)))
    // prepare answer
    var reveal: Seq[(P, Orientation.T)] = Nil
    var move: Seq[P] = Nil
    points.foreach(pp =>
      pp._2 match {
        case None => {
          move = move :+ pp._1.p
        }
        case Some(mess) => if (mess.m == M.POINTNOTREVEALED) reveal = reveal :+ (pointtoTile(pp._1.p), getOrientation(pp._1.p, p))
      }
    )
    Some(PossibleMove(p, reveal, move))
  }

  private def toActions(m: PossibleMove): Seq[Command.T] = {
    var li: Seq[Command.T] = Seq(Command.ENDOFMOVE)
    if (m == null) return li
    if (!m.reveal.isEmpty) li = li :+ Command.REVEALTILE
    if (!m.move.isEmpty) li = li :+ Command.MOVE
    li
  }

  // set city
  def itemizeForSetSity(b: GameBoard, civ: Civilization.T): Seq[P] =
    getFigures(b, civ).filter(_.s.figures.numberofScouts > 0).map(_.p).filter(p => SetCityAction.verifySetCity(b, civ, p, Command.SETCITY).isEmpty)

  def itemizeForSetCapital(b: GameBoard, civ: Civilization.T): Seq[P] =
    allSquares(b).filter(p => SetCityAction.verifySetCity(b, civ, p.p, Command.SETCAPITAL).isEmpty).map(_.p)

  // ========
  // external


  def allowedCommands(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    var cu: CurrentPhase = currentPhase(b)
    var co: List[Command.T] = Nil
    val count: (Int, Int) = getNumberOfArmies(b, civ)
    cu.turnPhase match {
      case TurnPhase.StartOfTurn => {
        if (!isCapitalBuild(b, civ)) return List(Command.SETCAPITAL)
        if (gameStart(b)) {
          if (count._1 == 0) co = co :+ Command.SETARMY
          if (count._2 == 0) co = co :+ Command.SETSCOUT
        } else if (!itemizeForSetSity(b, civ).isEmpty) co = co :+ Command.SETCITY

      }
      case TurnPhase.CityManagement => co = co ++ allowedActionForCityManagement(b, civ)
      case TurnPhase.Movement => {
        val move: Option[PossibleMove] = itemizeForMove(b, civ)
        if (move.isDefined) return toActions(move.get)
        co = co ++ allowedActionForMovement(b, civ)
      }
      case _ => {}
    }
    if (cu.notcompleted.find(_ == civ).isDefined) co = co ++ List(Command.ENDOFPHASE)
    return co
  }

  def itemizeCommandS(b: GameBoard, civ: Civilization.T, command: Command.T): String = {
    var pp: P = null
    var name: String = null
    var l: Seq[JsValue] = Nil
    command match {
      case Command.SETARMY | Command.SETSCOUT | Command.BUYARMY | Command.BUYSCOUT => {
        val a: Seq[(P, P)] = itemizeForSetBuyFigures(b, civ, command)
        l = a.map(o => Json.obj(S.p -> writesP(o._1), S.param -> writesP(o._2)))
        Json.prettyPrint(JsArray(l))
      }
      case Command.STARTMOVE => {
        var a: Seq[(Figures, P)] = itemizeforStartOfMove(b, civ)
        l = a.map(f => Json.obj(S.figures -> writesFigures(f._1), S.p -> writesP(f._2)))
      }
      case Command.MOVE => {
        val o: Option[PossibleMove] = itemizeForMove(b, civ)
        assert(o.isDefined && o.get != null)
        pp = o.get.p
        name = "moves"
        l = o.get.move.map(writesP(_))
      }
      case Command.REVEALTILE => {
        val o: Option[PossibleMove] = itemizeForMove(b, civ)
        assert(o.isDefined && o.get != null)
        pp = o.get.p
        name = "tiles"
        l = o.get.reveal.map(r => Json.obj(S.p -> writesP(r._1), S.orientation -> r._2))
      }
      case Command.SETCITY => {
        l = itemizeForSetSity(b, civ).map(writesP(_))
      }
      case Command.SETCAPITAL => {
        l = itemizeForSetCapital(b, civ).map(writesP(_))
      }
      case _ => None
    }
    if (pp == null) Json.prettyPrint(JsArray(l))
    else Json.prettyPrint(Json.obj(S.p -> writesP(pp), name -> JsArray(l)))
  }
}
