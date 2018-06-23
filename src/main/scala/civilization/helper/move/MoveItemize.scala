package civilization.helper.move

import civilization.gameboard.{Figures, GameBoard, PlayerDeck}
import civilization.helper._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.{writesFigures, writesP, _}
import civilization.message.{M, Mess}
import civilization.objects._
import play.api.libs.json.{JsValue, Json}


object MoveItemize extends ImplicitMiximFromJson with ImplicitMiximToJson {

  // Figure Movement

  case class SacrificeForTech(figure : P, city : P, civ : Civilization.T, tech : Seq[TechnologyName.T])

  private def convertSacrificeForTech(s: SacrificeForTech) : JsValue = Json.obj(
    S.p -> s.figure,
    S.city -> s.city,
    S.civ -> s.civ,
    S.tech -> s.tech
  )

  private def technologyToLearn(b: GameBoard, deck : PlayerDeck, foreign : Civilization.T) : Seq[TechnologyName.T] = {
    val yourlevel = techologyLevel(b, deck)
    if (yourlevel == 0) return Nil
    val youtech: Set[TechnologyName.T] = listOfLevelUpTo(b, deck, yourlevel).map(_.tech) toSet
    val foreigntech: Set[TechnologyName.T] = listOfLevelUpTo(b, b.playerDeck(foreign), yourlevel).map(_.tech).toSet
    (foreigntech -- youtech) toSeq
  }

  private def itemizeForFigureSacrifice(b: GameBoard, deck: PlayerDeck,itemi: Seq[(Figures, P)]): Seq[SacrificeForTech] = {
    if (!CivilizationFeatures.canSacrificeFigureForTech(deck.civ)) return Nil
    if (commandUsedAlready(b, deck.civ, TurnPhase.Movement, Command.SACRIFICEFIGUREFORTECH)) return Nil

    val itemi: Seq[(Figures, P)] = itemizeforStartOfMove(b, deck.civ)
    itemi.map(p => (p._2,allMoves(b,deck.civ,p._1,p._2,true))).
      flatMap(p => p._2.map((p._1,_))).
      map(pp => SacrificeForTech(pp._1,pp._2,getSquare(b,pp._2).civHere.get, technologyToLearn(b,deck,getSquare(b,pp._2).civHere.get))).
      filter(!_.tech.isEmpty)
  }

  def itemizeForFigureSacrifice(b: GameBoard, deck: PlayerDeck) : Seq[SacrificeForTech] = {
    val itemi: Seq[(Figures, P)] = itemizeforStartOfMove(b, deck.civ)
    itemizeForFigureSacrifice(b,deck,itemi)
  }


  private def allowedActionForMovement(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    val itemi: Seq[(Figures, P)] = itemizeforStartOfMove(b, civ)
    if (itemi.isEmpty) Nil
    else {
      var res: Seq[Command.T] = Seq(Command.STARTMOVE)
      if (!itemizeForFigureSacrifice(b,b.playerDeck(civ),itemi).isEmpty) res = res :+ Command.SACRIFICEFIGUREFORTECH
      res
    }
  }

  /** not private, used in test */
  def itemizeforStartOfMove(b: GameBoard, civ: Civilization.T): Seq[(Figures, P)] = {
    val lastm: Map[P, Figures] = finishedAtPoint(b, civ)
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
  case class PossibleMove(val p: P, val endofmove: Boolean, val reveal: Seq[(P, Orientation.T)], val move: Seq[P],
                          val hut: Option[P], val enemy: Seq[P])

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
    // only ENDOFMOVE possible
    val last: PlayerMove = ll.last
    val p: P = last.lastp
    val canstay: Boolean = checkFinalPoint(b, civ, getSquare(b, p), last.f.toFigures).isEmpty
    if (last.len == lim.travelSpeed) return Some(PossibleMove(p, canstay, Nil, Nil, None, Nil))
    // only direct point, 4 directions
    val around: Seq[MapSquareP] = squaresAround(b, p)
    val points: Seq[(MapSquareP, Option[Mess])] = around.map(p => (p, figureMovePointCheck(b, civ, toFiguresParam(last), p.p, last.len + 1 == lim.travelSpeed)))
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
          if (mess.m == M.CANNOTSETFIGUREONVILLAGE || mess.m == M.CANNOTSETFIGUREONALIENCIV /* || mess.m == M.CANNOTSETFGUREONALIENCITY || mess.m == M.CANNOTSETFIGUREONALIENCIV */ ) {
            val figo: Option[PlayerMove] = getCurrentMove(b, civ)
            if (figo.get.f.numberofArmies > 0)
              enemy = enemy :+ pp._1.p
          }
      }
    )
    Some(PossibleMove(p, canstay, reveal, move, hut, enemy))
  }

  private def toActions(m: PossibleMove): Seq[Command.T] = {
    var li: Seq[Command.T] = if (m.endofmove) Seq(Command.ENDOFMOVE) else Nil
    if (!m.reveal.isEmpty) li = li :+ Command.REVEALTILE
    if (!m.move.isEmpty) li = li :+ Command.MOVE
    if (m.hut.isDefined) li = li :+ Command.EXPLOREHUT
    if (!m.enemy.isEmpty) li = li :+ Command.ATTACK
    if (li.isEmpty) li = Seq(Command.KILLFIGURE)
    li
  }

  def allowedCommands(b: GameBoard, civ: Civilization.T): (Boolean, Seq[Command.T]) = {
    val move: Option[PossibleMove] = itemizeForMove(b, civ)
    if (move.isDefined) return (true, toActions(move.get))
    (false, allowedActionForMovement(b, civ))
  }

  def itemizeCommand(b: GameBoard, deck : PlayerDeck, command: Command.T): (String, P, Seq[JsValue]) = {
    var pp: P = null
    var name: String = null
    var l: Seq[JsValue] = Nil
    command match {
      case Command.SACRIFICEFIGUREFORTECH => {
        var itemi: Seq[(Figures, P)] = itemizeforStartOfMove(b, deck.civ)
        l = itemizeForFigureSacrifice(b,deck,itemi).map(convertSacrificeForTech)
      }
      case Command.STARTMOVE => {
        val a: Seq[(Figures, P)] = itemizeforStartOfMove(b, deck.civ)
        l = a.map(f => Json.obj(S.figures -> writesFigures(f._1), S.p -> writesP(f._2)))
      }
      case Command.MOVE => {
        val o: Option[PossibleMove] = itemizeForMove(b, deck.civ)
        assert(o.isDefined && o.get != null)
        pp = o.get.p
        name = "moves"
        l = o.get.move.map(writesP(_))
      }
      case Command.REVEALTILE => {
        val o: Option[PossibleMove] = itemizeForMove(b, deck.civ)
        assert(o.isDefined && o.get != null)
        pp = o.get.p
        name = "tiles"
        l = o.get.reveal.map(r => Json.obj(S.p -> writesP(r._1), S.orientation -> r._2))
      }
      case Command.EXPLOREHUT => {
        val o: Option[PossibleMove] = itemizeForMove(b, deck.civ)
        assert(o.isDefined && o.get != null)
        assert(o.get.hut.isDefined)
        name = "explore"
        pp = o.get.p
        l = List(writesP(o.get.hut.get))
      }
      case Command.ATTACK => {
        val o: Option[PossibleMove] = itemizeForMove(b, deck.civ)
        assert(o.isDefined && o.get != null)
        assert(!o.get.enemy.isEmpty)
        name = "attack"
        pp = o.get.p
        l = o.get.enemy
      }
    }
    (name, pp, l)
  }

}
