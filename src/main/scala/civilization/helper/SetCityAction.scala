package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage, constructCommand}
import civilization.gameboard.{Figures, GameBoard, PlayerDeck}
import civilization.helper.move.MoveAction
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.io.tojson.{ImplicitMiximToJson, writesFigures}
import play.api.libs.json.JsValue


object SetCityAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.SETCAPITAL, Command.SETCITY)

  private def verifySetCity(board: GameBoard, civ: Civilization.T, p: P, command: Command.T): Option[Mess] = {

    val deck: PlayerDeck = board.playerDeck(civ)
    val mapp: MapSquareP = getSquare(board, p)
    command match {
      case Command.SETCAPITAL =>
        if (isCapitalBuild(board, civ)) return Some(Mess(M.CAPITALALREADYBUILD))
        if (!mapp.t.tile.civhome || mapp.t.tile.civ.get != civ)
          return Some(Mess(M.CAPITALCANBEBUILDONLYONHOMETILE, p))
      case Command.SETCITY => {
        if (getLimits(board, civ).citieslimit == 0) return Some(Mess(M.CITYLIMITEXCEEDED))
        if (!mapp.s.figures.civOccupying(civ) || mapp.s.figures.numberofScouts == 0) return Some(Mess(M.CITYSHOULDBEBUILDONSCOUT, p))
      }
    }
    isSquareForCity(board, p, civ)
  }

  protected class SetCityAction extends AbstractCommand {

    private def setcitycommandverify(board: GameBoard, civ: Civilization.T, p: P, command: Command.T): Mess = {
      verifySetCity(board, civ, p, command).getOrElse(null)
    }

    private def setcitycommandexecute(board: GameBoard, civ: Civilization.T, p: P, command: Command.T) = {
      val sq: MapSquareP = getSquare(board, p)
      // build city
      if (command == Command.SETCAPITAL) {
        sq.s.city = Some(City(civ, City.Capital))
        if (isExecute && CivilizationFeatures.buildWallInCapital(civ)) {
          val command: Command = constructCommand(Command.BUILDCITYWALLFORFREE, civ, p)
          board.addForcedCommand(command)
        }
        if (isExecute && CivilizationFeatures.freeWonderOfTheWorldAtTheBeginning(civ)) {
          val wonder : Wonders.T = getRandomAncientWonder(board)
          val command: Command = constructCommand(Command.RANDOMWONDER, civ, p, wonder)
          board.addForcedCommand(command)
        }
      }
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
        // 2017/12/25 : isExecute
        if (!f.empty && isExecute) {
          // try to position them around
          // checkFinalPoint

          val moveto: Option[P] = squaresAround(board, p).
            filter(po => isSquareForFigure(board, civ, Figure.Army, po.p).isEmpty &&
              MoveAction.checkFinalPoint(board, civ, po, f).isEmpty).
            map(_.p).headOption

          if (moveto.isDefined) {
            val command: Command = constructCommand(Command.FORCEDMOVEFIGURES, civ, moveto.get, writesFigures(f))
            // execute later
            board.addForcedCommand(command)
          }
        }
      }
    }


    override def execute(board: GameBoard) = {
      setcitycommandexecute(board, civ, p, command)
      advanceCultureForFree(board,civ,isExecute)
    }

    override def verify(board: GameBoard): Mess = setcitycommandverify(board, civ, p, command)
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new SetCityAction

  // set city
  private def itemizeForSetCity(b: GameBoard, civ: Civilization.T): Seq[P] =
    getFigures(b, civ).filter(_.s.figures.numberofScouts > 0).map(_.p).filter(p => verifySetCity(b, civ, p, Command.SETCITY).isEmpty)

  // capital
  private def itemizeForSetCapital(b: GameBoard, civ: Civilization.T): Seq[P] =
    allSquares(b).filter(p => verifySetCity(b, civ, p.p, Command.SETCAPITAL).isEmpty).map(_.p)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = {
    if (com == Command.SETCITY) itemizeForSetCity(b, civ)
    else itemizeForSetCapital(b, civ)
  }

}