package civilization.helper

import civilization.action
import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard.{Figures, GameBoard, PlayerDeck}
import civilization.helper.SetCityAction.SetCityAction
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.Mess
import civilization.objects._
import play.api.libs.json.JsValue


object SetFigureAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.SETARMY, Command.SETSCOUT, Command.BUYARMY, Command.BUYSCOUT)

  // public because it is tested externally
  def itemizeForSetBuyFigures(b: GameBoard, deck : PlayerDeck, com: Command.T): Seq[(P, P)] = {
    if (firstRound(b,Some(TurnPhase.StartOfTurn))) {
      if (com == Command.BUYSCOUT || com == Command.BUYARMY) return Nil
      val count: (Int, Int) = getNumberOfArmies(b, deck)
      if (com == Command.SETARMY  && count._1 >= CivilizationFeatures.numberofArmiesToStart(deck)) return Nil
      if (com == Command.SETSCOUT  && count._2 >= CivilizationFeatures.numberofScoutsToStart(deck)) return Nil
    }
    else // !gameStart
      if (com == Command.SETSCOUT || com == Command.SETARMY) return Nil

    val fi: Figure.T = if (com == Command.SETARMY || com == Command.BUYARMY) Figure.Army else Figure.Scout

    val lcities: Seq[P] = if (com == Command.BUYSCOUT || com == Command.BUYARMY) CitiesCanAfford(b, deck, ObjectCost.getCost(fi)) else citiesForCivilization(b, deck).map(_.p)
    var alist: Seq[(P, P)] = lcities.flatMap(s => pointsAround(b, s).map(p => (s, p)).filter(po => isSquareForFigure(b, deck, fi, po._2).isEmpty))
    alist
  }


  def verifySetFigure(board: GameBoard, deck : PlayerDeck, pcity: P, p: P, f: Figure.T, command: Command.T): Option[Mess] = {
    var m: Option[Mess] = checkCity(board, pcity)
    if (m.isDefined) return m
    if (command == Command.BUYSCOUT || command == Command.BUYARMY) {
      m = canBuyFigure(board, deck, pcity, f)
      if (m.isDefined) return m
    }
    checkP(board, p)
    isSquareForFigure(board, deck, f, p)
  }

  class SetFigureAction(override val param: (Figure.T, P)) extends AbstractCommand(param) {

    def execute(board: GameBoard) =
      putFigures(board, civ, param._2, if (param._1 == Figure.Army) Figures(1, 0) else Figures(0, 1))

    def verify(board: GameBoard): Mess = verifySetFigure(board, deck, p, param._2, param._1, command).getOrElse(null)
  }

  override def itemize(b: GameBoard, deck : PlayerDeck, com: Command.T): Seq[JsValue] = itemizeForSetBuyFigures(b, deck, com)

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new SetFigureAction(if (command == Command.SETARMY || command == Command.BUYARMY) Figure.Army else Figure.Scout, param)
}
