package civilization.helper

import civilization.action.AbstractCommand
import civilization.gameboard.{Figures, GameBoard}
import civilization.message.{M, Mess}
import civilization.objects._

object MoveAction {


  private def directMove(p: P, next: P): Boolean = {
    // removed: 2017.08.21
    //    if (p.row == next.row && p.col == next.col) return true
    if ((p.row == next.row) && ((p.col + 1 == next.col) || (p.col - 1 == next.col))) return true
    if ((p.col == next.col) && ((p.row + 1 == next.row) || (p.row - 1 == next.row))) return true
    false
  }

  private def startOfMoveVerify(b: GameBoard, civ: Civilization.T, p: P, fig: Figures): Mess = {
    checkP(b, p)
    // check if figures moves again from last position
    val s: MapSquareP = getSquare(b, p)
    val flast: Seq[PlayerMove] = civLastMoves(b, civ)
    val f = flast.find(_.lastp == p)
    if (f.isDefined) {
      // TODO: should be amended
      // scenario:
      // 1. Figures at position p
      // 2. Another figures stops at position p
      // 3. Move first figures from position p
      // 4. Implementation now covers only specific situation: before scouts and stop armies
      // 5. Does not cover: 1. 1 army before 2. 1 army stop 3. first army wants to move
      if (f.get.f.numberofArmies == fig.numberofArmies && f.get.f.numberofScouts == fig.numberofScouts)
        return Mess(M.FIGURESMOVEDAGAIN, (f.get.f.numberofArmies, f.get.f.numberofScouts))
    }

    if (s.s.figures.civ != null && s.s.figures.civ != civ) return Mess(M.NOFIGURESBELONGINGTOCIVILIZATION, (p, civ))
    if (s.s.figures.numberofScouts < fig.numberofScouts) return Mess(M.NUMBEROFSCOUTSLESS, (p, s.s.figures.numberofScouts, fig.numberofScouts))
    if (s.s.figures.numberofArmies < fig.numberofArmies) return Mess(M.NUMBEROFARMIESLESS, (p, s.s.figures.numberofArmies, fig.numberofArmies))
    null
  }

  def checkFinalPoint(b: GameBoard, civ: Civilization.T, s: MapSquareP, fig: Figures): Option[Mess] = {
    val li: PlayerLimits = getLimits(b, civ)
    val figdesc: (P, Figures) = (s.p, fig)
    if (s.sm.terrain == Terrain.Water && li.waterstopallowed) return Some(Mess(M.CANNOTSTOPINWATER, figdesc))
    if (s.s.cityhere && s.s.city.get.belongsTo(civ)) return Some(Mess(M.CANNOTSTOPINCITY, figdesc))
    // 2017/08/28 figures already on the point
    //    val numb = fig.numberofArmies + fig.numberofScouts + s.s.figures.numberofArmies + s.s.figures.numberofScouts
    //    if (numb > li.stackinglimit) return Some(Mess(M.STACKINGSIZEEXCEEDED, (figdesc, li.stackinglimit, numb)))
    None
  }

  def figureMovePointCheck(b: GameBoard, civ: Civilization.T, fig: PlayerMove, p: P, endofmove: Boolean): Option[Mess] = {
    assert(p != null || endofmove)
    if (p == null) return checkFinalPoint(b, civ, getSquare(b, fig.lastp), fig.f.toFigures)
    val figdesc: (P, PlayerMove) = (p, fig)
    // enfofmove can be the same point as last
    if (!endofmove || fig.lastp != p)
      if (!directMove(fig.lastp, p)) return Some(Mess(M.MOVENOTCONSECUTIVE, figdesc))
    val li: PlayerLimits = getLimits(b, civ)
    // ignore first STARTMOVE
    if (fig.len + 1 > li.travelSpeed) return Some(Mess(M.TRAVELSPEEDEXCEEDED, (figdesc, li.travelSpeed)))
    //    if (!endofmove && fig.len + 1 > li.travelSpeed) return Mess(M.SHOULDBEDNFOFMOVENOW, (figdesc, li.travelSpeed))
    val s: MapSquareP = getSquare(b, p)
    if (!s.revealed) return Some(Mess(M.POINTNOTREVEALED, p))
    if (s.sm.terrain == Terrain.Water && !li.watercrossingallowed) return Some(Mess(M.CANNOTCROSSWATER, figdesc))
    val mess: Option[Mess] = isSquareForFigures(b, civ, fig.f.toFigures, s.s, li)
    if (mess.isDefined) return mess
    if (endofmove) checkFinalPoint(b, civ, s, fig.f.toFigures) else None
  }

  private def figureMoveVerify(b: GameBoard, civ: Civilization.T, p: P, endofmove: Boolean): Option[Mess] = {
    val figo: Option[PlayerMove] = getCurrentMove(b, civ)
    if (figo.isEmpty) return Some(Mess(M.CANNOTFINDSTARTOFMOVE, p))
    val fig: PlayerMove = figo.get
    if (fig == null) return Some(Mess(M.CANNOTFINDSTARTOFMOVE, p))
    figureMovePointCheck(b, civ, fig, p, endofmove)
  }

  private def figureMoveExecute(b: GameBoard, civ: Civilization.T, p: P, endofmove: Boolean, f: Option[Figures]) =
    if (endofmove && p == null) None
    else
      moveFigures(b, civ, p, f)

  class StartMoveAction(override val param: Figures) extends AbstractCommand(param) {
    def execute(board: GameBoard) = Unit

    def verify(board: GameBoard): Mess = startOfMoveVerify(board, civ, p, param)
  }

  class MoveAction(override val param: Figures) extends AbstractCommand(param) {

    def execute(board: GameBoard) = figureMoveExecute(board, civ, p, false, if (param == null) None else Some(param))

    def verify(board: GameBoard): Mess = figureMoveVerify(board, civ, p, false).getOrElse(null)
  }

  class EndOfMoveAction(override val param: Figures) extends AbstractCommand(param) {

    def execute(board: GameBoard) = figureMoveExecute(board, civ, p, true, if (param == null) None else Some(param))

    def verify(board: GameBoard): Mess = figureMoveVerify(board, civ, p, true).getOrElse(null)
  }

  class ForceMoveAction(override val param: Figures) extends AbstractCommand(param) {
    def execute(board: GameBoard) = putFigures(board, civ, p, param)

    def verify(board: GameBoard): Mess = checkFinalPoint(board, civ, getSquare(board, p), param).getOrElse(null)
  }

}