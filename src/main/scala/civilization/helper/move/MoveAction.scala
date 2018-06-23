package civilization.helper.move

import civilization.action.{AbstractCommand, Command, constructCommand}
import civilization.gameboard.{Figures, GameBoard}
import civilization.helper._
import civilization.helper.move.MoveItemize.SacrificeForTech
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{J, M, Mess}
import civilization.objects._

object MoveAction extends ImplicitMiximFromJson with ImplicitMiximToJson {


  private def figuresToMove(b: GameBoard, civ: Civilization.T, p: P) : Figures = {
    val s: MapSquareP = getSquare(b, p)
    val exiF : Figures = s.s.figures.toFigures
    val lastm: Map[P, Figures] = finishedAtPoint(b,civ)
    if (lastm contains  p) exiF - lastm.get(p).get
    exiF
  }

  private def startOfMoveVerify(b: GameBoard, civ: Civilization.T, p: P, fig: Figures): Mess = {
    checkP(b, p)
    // check if figures moves again from last position
//    val s: MapSquareP = getSquare(b, p)
/*`
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
*/
    val ss: MapSquareP = getSquare(b, p)
    val exiF : Figures = figuresToMove(b,civ,p)
    if (ss.s.figures.civ != null && ss.s.figures.civ != civ) return Mess(M.NOFIGURESBELONGINGTOCIVILIZATION, (p, civ))
    if (exiF.numberofScouts < fig.numberofScouts) return Mess(M.NUMBEROFSCOUTSLESS, (p, exiF.numberofScouts, fig.numberofScouts))
    if (exiF.numberofArmies < fig.numberofArmies) return Mess(M.NUMBEROFARMIESLESS, (p, exiF.numberofArmies, fig.numberofArmies))
    null
  }


  private def figureMoveVerify(b: GameBoard, civ: Civilization.T, p: P, endofmove: Boolean): Option[Mess] = {
    val figo: Option[PlayerMove] = getCurrentMove(b, civ)
    if (figo.isEmpty) return Some(Mess(M.CANNOTFINDSTARTOFMOVE, p))
    val fig: PlayerMove = figo.get
    if (fig == null) return Some(Mess(M.CANNOTFINDSTARTOFMOVE, p))
    figureMovePointCheck(b, civ, toFiguresParam(fig), p, endofmove)
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

  class KillFigureAction(override val param: Figures) extends AbstractCommand(param) {
    def execute(board: GameBoard) = {
      moveFigures(board, civ, p, if (param == null) None else Some(param), true)
      addToJournal(board, civ, J.FIGURESAREKILLED, null)
    }

    def verify(board: GameBoard): Mess = null
  }

  class SacrificeFigureFortech(override val param: TechnologyName.T) extends AbstractCommand(param) {
    def execute(board: GameBoard) = {
      val ss: MapSquareP = getSquare(board, p)
      // army or scout
      val tom : Figures = figuresToMove(board,civ,p)
      val fig = if (tom.numberofArmies > 0) Figures(1,0) else Figures(0,1)
      // sacrifice figure
      putFigures(board,civ,p,-fig)
      // learn tech
      if (isExecute) {
        val commandC = constructCommand(Command.RESEARCHFREETECHNOLOGY, civ, null,param)
        board.addForcedCommand(commandC)
      }
    }

    def verify(board: GameBoard): Mess = {
      val i : Seq[SacrificeForTech] = MoveItemize.itemizeForFigureSacrifice(board,deck)
      if (!i.exists( s => s.figure == p && (s.tech contains param))) Mess(M.CANNOTSACRIFICEFIGUREFORFREETECH,param)
      else null
    }
  }

}