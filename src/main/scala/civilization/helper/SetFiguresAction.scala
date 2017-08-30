package civilization.helper

import civilization.action.AbstractCommand
import civilization.gameboard.{GameBoard, Figures}
import civilization.message.Mess
import civilization.objects.{Command, Figure, P, Civilization}


object SetFigureAction {

  def verifySetFigure(board: GameBoard, civ: Civilization.T, pcity: P, p: P, f: Figure.T, command: Command.T): Option[Mess] = {
    var m: Option[Mess] = checkCity(board, pcity)
    if (m.isDefined) return m
    if (command == Command.BUYSCOUT || command == Command.BUYARMY) {
      m = canBuyFigure(board, civ, pcity, f)
      if (m.isDefined) return m
    }
    checkP(board, p)
    isSquareForFigure(board, civ, f, p)
  }

  class SetFigureAction(override val param: (Figure.T, P)) extends AbstractCommand(param) {

    def execute(board: GameBoard) =
      putFigures(board, civ, param._2, if (param._1 == Figure.Army) Figures(1, 0) else Figures(0, 1))

    def verify(board: GameBoard): Mess = verifySetFigure(board, civ, p, param._2, param._1, command).getOrElse(null)
  }

}
