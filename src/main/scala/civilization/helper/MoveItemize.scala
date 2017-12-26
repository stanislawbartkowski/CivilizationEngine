package civilization.helper

import civilization.gameboard.{Figures, GameBoard}
import civilization.io.tojson.{writesFigures, writesP}
import civilization.message.{M, Mess}
import civilization.objects._
import play.api.libs.json.{JsValue, Json}
import civilization.io.tojson._


object MoveItemize {

  // Figure Movement

  private def allowedActionForMovement(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    val itemi: Seq[(Figures, P)] = itemizeforStartOfMove(b: GameBoard, civ: Civilization.T)
    if (itemi.isEmpty) Nil else Seq(Command.STARTMOVE)
  }

  /** not private, used in test */
  def itemizeforStartOfMove(b: GameBoard, civ: Civilization.T): Seq[(Figures, P)] = {
    val lastp: Seq[(Figures, P, P)] = civLastMoves(b, civ).map(o => (o.f.toFigures, o.begstop._1.p.get, o.begstop._2.p.get))
    //TODO: can be done better,2 traversals, use mutable map and fold
    //    val startmap : Map[P,Figures] = lastp.map(t => t._3 -> Figures(0,0)) toMap
    //    val lastm: Map[P, Figures] = lastp.map(t => t._3 -> t._1).toMap
    // current points on board belonging to civilization
    //    val current: Seq[(Figures, P)] = allSquares(b).filter(p => p.s.figures.civOccupying.isDefined && p.s.figures.civOccupying.get == civ).map(m => (m.s.figures.toFigures, m.p))
    // sum all figures finishing at given point
    val lastm: Map[P, Figures] = lastp.groupBy(_._3).map(e => e._1 -> e._2.foldLeft[Figures](Figures(0, 0))((f, p) => {
      f + p._1;
      f
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
  /** not private, used in test */
  case class PossibleMove(var p: P, val reveal: Seq[(P, Orientation.T)], val move: Seq[P], val hut: Option[P], val enemy: Seq[P])

  // return
  //  None : no move
  //  Some(null) : ENDMOVE
  //  Some(PossibleMove) : MOVE
  // TODO: I'm not happy with that design
  /** not private, used in test */
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
    var hut: Option[P] = None
    var enemy: Seq[P] = Nil
    points.foreach(pp =>
      pp._2 match {
        case None => {
          move = move :+ pp._1.p
        }
        case Some(mess) =>
          if (mess.m == M.POINTNOTREVEALED) reveal = reveal :+ (pointtoTile(pp._1.p), getOrientation(pp._1.p, p))
          if (mess.m == M.CANNOTSETFIGUREONHUT) {
            val figo: Option[PlayerMove] = getCurrentMove(b, civ)
            if (figo.get.f.numberofArmies > 0 || (figo.get.f.numberofScouts > 0 && lim.scoutscanExplore))
              hut = Some(pp._1.p)
          }
          if (mess.m == M.CANNOTSETFIGUREONVILLAGE  || mess.m == M.CANNOTSETFIGUREONALIENCIV /* || mess.m == M.CANNOTSETFGUREONALIENCITY || mess.m == M.CANNOTSETFIGUREONALIENCIV */ ) {
            val figo: Option[PlayerMove] = getCurrentMove(b, civ)
            if (figo.get.f.numberofArmies > 0)
              enemy = enemy :+ pp._1.p
          }
      }
    )
    Some(PossibleMove(p, reveal, move, hut, enemy))
  }

  private def toActions(m: PossibleMove): Seq[Command.T] = {
    var li: Seq[Command.T] = Seq(Command.ENDOFMOVE)
    if (m == null) return li
    if (!m.reveal.isEmpty) li = li :+ Command.REVEALTILE
    if (!m.move.isEmpty) li = li :+ Command.MOVE
    if (m.hut.isDefined) li = li :+ Command.EXPLOREHUT
    if (!m.enemy.isEmpty) li = li :+ Command.ATTACK
    li
  }

  def allowedCommands(b: GameBoard, civ: Civilization.T): (Boolean, Seq[Command.T]) = {
    val move: Option[PossibleMove] = itemizeForMove(b, civ)
    if (move.isDefined) return (true, toActions(move.get))
    (false,allowedActionForMovement(b, civ))
  }

  def itemizeCommand(b: GameBoard, civ: Civilization.T, command: Command.T): (String, P, Seq[JsValue]) = {
    var pp: P = null
    var name: String = null
    var l: Seq[JsValue] = Nil
    command match {
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
      case Command.EXPLOREHUT => {
        val o: Option[PossibleMove] = itemizeForMove(b, civ)
        assert(o.isDefined && o.get != null)
        assert(o.get.hut.isDefined)
        name = "explore"
        pp = o.get.p
        l = List(writesP(o.get.hut.get))
      }
      case Command.ATTACK => {
        val o: Option[PossibleMove] = itemizeForMove(b, civ)
        assert(o.isDefined && o.get != null)
        assert(!o.get.enemy.isEmpty)
        name = "attack"
        pp = o.get.p
        l = o.get.enemy
      }
    }
    (name,pp,l)
  }

}
