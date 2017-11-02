package civilization.helper

import civilization.gameboard.{Figures, GameBoard}
import civilization.io.tojson._
import civilization.message._
import civilization.objects._
import civilization.helper.SetFigureAction.itemizeForSetBuyFigures
import play.api.libs.json.{JsArray, JsValue, Json}
import civilization.action.CommandContainer

object AllowedCommands {

  private def allowedActionForCityManagement(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    var cu: Seq[Command.T] = Nil
    if (!itemizeForSetBuyFigures(b, civ, Command.BUYSCOUT).isEmpty) cu = cu :+ Command.BUYSCOUT
    if (!itemizeForSetBuyFigures(b, civ, Command.BUYARMY).isEmpty) cu = cu :+ Command.BUYARMY
    cu
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
    var co: Seq[Command.T] = Nil
    // if "turned" phase of the game, only current player has moves until completes
    if (TurnPhase.turnAction(cu.turnPhase) && civ != cu.notcompleted.head) return Nil
    co = CommandContainer.commandsAvail(b, civ, cu.turnPhase)
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
        val r = MoveItemize.allowedCommands(b, civ)
        if (r._1) return co ++ r._2
        co = co ++ r._2
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
    if (CommandContainer.isCommandCovered(command))
      return Json.prettyPrint(JsArray(CommandContainer.itemize(b, civ, command)))
    if (Command.actionPhase(command) == TurnPhase.Movement) {
      val r = MoveItemize.itemizeCommand(b, civ, command)
      pp = r._2
      name = r._1
      l = r._3
    }
    else
      command match {
        case Command.SETARMY | Command.SETSCOUT | Command.BUYARMY | Command.BUYSCOUT => {
          val a: Seq[(P, P)] = itemizeForSetBuyFigures(b, civ, command)
          l = a.map(o => Json.obj(S.p -> writesP(o._1), S.param -> writesP(o._2)))
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
