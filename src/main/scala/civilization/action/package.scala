package civilization

import civilization.gameboard.GameBoard
import civilization.objects._
import civilization.message.{FatalError, M, Mess}
import civilization.io.fromjson._
import civilization.helper._
import play.api.libs.json.{JsArray, JsValue}

package object action {

  def constructCommand(c: CommandValues): Command =
    constructCommand(c.command, c.civ, c.p, c.param)


  def constructCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue = null): Command = {
    assert(civ != null && command != null)
    val c: Command = produceCommand(command, civ, p, param)
    c.command = command
    c.p = p
    c.civ = civ
    c.j = param
    c
  }

  private def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = command match {

    case Command.SETCAPITAL | Command.SETCITY => new SetCityAction.SetCityAction()

    case Command.SETSCOUT | Command.SETARMY => {
      val p: P = toP(param)
      new SetFigureAction.SetFigureAction(if (command == Command.SETARMY) Figure.Army else Figure.Scout, p)
    }

    case Command.BUYSCOUT | Command.BUYARMY => {
      val p: P = toP(param)
      new SetFigureAction.SetFigureAction(if (command == Command.BUYARMY) Figure.Army else Figure.Scout, p)
    }

    case Command.FORCEDMOVEFIGURES => new MoveAction.ForceMoveAction(toFigures(param))


    case Command.ENDOFPHASE => {

      val phase: TurnPhase.T = toTurnPhase(param)

      if (phase == TurnPhase.Research) new EndOfResearchAction
      else
        new AbstractCommand(phase) {
          override def execute(board: GameBoard) = Unit

          override def verify(board: GameBoard): Mess = null
        }
    }

    case Command.STARTMOVE => new MoveAction.StartMoveAction(toFigures(param))


    case Command.MOVE => new MoveAction.MoveAction()

    case Command.ENDOFMOVE => new MoveAction.EndOfMoveAction()

    case Command.SPENDTRADE => new SpendTrade.SpendTrade(toInt(param))

    case Command.UNDOSPENDTRADE =>
      new AbstractCommandNone() {
        override def execute(board: GameBoard) = Unit

        override def verify(board: GameBoard): Mess = null
      }

    case Command.UNDOSENDPRODUCTION =>
      new AbstractCommand(toP(param)) {
        override def execute(board: GameBoard) = Unit

        override def verify(board: GameBoard): Mess = null
      }

    case Command.SENDPRODUCTION => new SendProduction.SendProduction(toP(param))

    case Command.REVEALTILE => new RevealTileAction(toOrientation(param))

    case Command.RESEARCH => new ResearchTechnologyAction(toTechnologName(param))

    case _ => throw FatalError(Mess(M.NOTIMPLELEMENTEDYET, command))
  }

  trait Command {

    // TODO: review
    // var only because of constructCommand, consider better solution
    // immutable outside
    var command: Command.T = _

    var civ: Civilization.T = _

    var canceled: Boolean = false
    var p: P = _
    var j: JsValue = _

    def setCanceled = canceled = true

    def param: Any

    def param1: Any

    // TODO: should return Option[Mess], not null
    // null : success, Mess : failure and failure info
    def verify(board: GameBoard): Mess

    def execute(board: GameBoard)
  }

  abstract class AbstractCommandNone(val param: Any = null, val param1: Any = null) extends Command

  abstract class AbstractCommand[T](val param: T = null, val param1: Any = null) extends Command

  abstract class AbstractCommand1[T, T1](val param: T = null, var param1: T1 = null) extends Command

  trait CommandPackage {
    def getSet: Set[Command.T]

    def commandsAvail(b: GameBoard, civ: Civilization.T): Seq[Command.T]

    def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): JsArray

    def produceCommand(par: JsValue): Command
  }

}
