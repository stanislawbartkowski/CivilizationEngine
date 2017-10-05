package civilization.helper

import civilization.action.AbstractCommand
import civilization.gameboard.{Figures, GameBoard}
import civilization.message.Mess
import civilization.objects._


object SetFigureAction {

  def itemizeForSetBuyFigures(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[(P, P)] = {
    val fi: Figure.T = if (com == Command.SETARMY || com == Command.BUYARMY) Figure.Army else Figure.Scout

    val lcities : Seq[P] = if (com == Command.BUYSCOUT || com == Command.BUYARMY) CitiesCanAfford(b,civ,ObjectCost.getCost(fi)) else citiesForCivilization(b, civ).map(_.p)
    var alist: Seq[(P, P)] = lcities.flatMap(s => pointsAround(b, s).map(p => (s, p)).filter(po => isSquareForFigure(b, civ, fi, po._2).isEmpty))
    alist
  }


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
