package civilization.helper

import civilization.action.{AbstractCommand, Command, constructCommand}
import civilization.gameboard.{Figures, GameBoard, PlayerDeck}
import civilization.message.{M, Mess}
import civilization.objects.{City, Civilization, Command, P}
import civilization.io.tojson.writesFigures

object SetCityAction {

  def verifySetCity(board: GameBoard, civ: Civilization.T, p: P, command: Command.T): Option[Mess] = {

    val deck: PlayerDeck = board.playerDeck(civ)
    command match {
      case Command.SETCAPITAL => if (isCapitalBuild(board, civ)) return Some(Mess(M.CAPITALALREADYBUILD))
      case Command.SETCITY => {
        if (getLimits(board, civ).citieslimit == 0) return Some(Mess(M.CITYLIMITEXCEEDED))
        val mapp: MapSquareP = getSquare(board, p)
        if (!mapp.s.figures.civOccupying(civ) || mapp.s.figures.numberofScouts == 0) return Some(Mess(M.CITYSHOULDBEBUILDONSCOUT, p))
      }
    }
    isSquareForCity(board, p,civ)
  }

  class SetCityAction extends AbstractCommand {

    private def setcitycommandverify(board: GameBoard, civ: Civilization.T, p: P, command: Command.T): Mess = {
      verifySetCity(board, civ, p, command).getOrElse(null)
    }

    private def setcitycommandexecute(board: GameBoard, civ: Civilization.T, p: P, command: Command.T) = {
      val sq: MapSquareP = getSquare(board, p)
      // build city
      if (command == Command.SETCAPITAL) sq.s.city = Some(City(civ, City.Capital))
      else {
        // only for regular city
        sq.s.city = Option(City(civ, City.Normal))
        // remove scout
        val f: Figures = sq.s.figures.toFigures
        // kill all figures at the place of the city
        sq.s.figures.numberofScouts = sq.s.figures.numberofScouts - 1
        // move stacked figures somewhere
        // kill all figures at the place of city
        sq.s.figures.kill()
        // remove one scout
        f.numberofScouts = f.numberofScouts - 1
        if (!f.empty) {
          // try to position them around
          // checkFinalPoint

          val moveto: Option[P] = squaresAround(board, p).filter(po => MoveAction.checkFinalPoint(board, civ, po, f).isEmpty).map(_.p).headOption
          if (moveto.isDefined) {
            val command: Command = constructCommand(Command.FORCEDMOVEFIGURES, civ, moveto.get, writesFigures(f))
            // execute later
            board.addForcedCommand(command)
          }
        }
      }
    }


    override def execute(board: GameBoard) = setcitycommandexecute(board, civ, p, command)

    override def verify(board: GameBoard): Mess = setcitycommandverify(board, civ, p, command)
  }

}