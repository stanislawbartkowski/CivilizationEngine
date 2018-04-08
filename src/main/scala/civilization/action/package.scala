package civilization

import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.helper.battle.AttackCommand
import civilization.helper.move.{ExploreHutCommand, MoveAction, RevealTileAction}
import civilization.io.fromjson._
import civilization.io.tojson._
import civilization.message.{FatalError, M, Mess}
import civilization.objects._
import play.api.libs.json.{JsNull, JsValue}

package object action extends ImplicitMiximToJson with ImplicitMiximFromJson {

  def constructCommand(c: CommandValues): Command =
    constructCommand(c.command, c.civ, c.p, c.param)

  def enrich(c: Command, command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = {
    c.command = command
    c.p = p
    c.civ = civ
    c.j = param
    c
  }


  def constructCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue = null): Command = {
    assert(civ != null && command != null)
    val c: Command = if (CommandContainer.isCommandCovered(command)) CommandContainer.produceCommand(command, civ, p, param) else produceCommand(command, civ, p, param)
    enrich(c, command, civ, p, param)
  }

  private def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = command match {


//    case Command.SETCAPITAL | Command.SETCITY => new SetCityAction.SetCityAction()

//    case Command.SETSCOUT | Command.SETARMY => {
//      val p: P = toP(param)
//      new SetFigureAction.SetFigureAction(if (command == Command.SETARMY) Figure.Army else Figure.Scout, p)
//    }

  //  case Command.BUYSCOUT | Command.BUYARMY => {
//      val p: P = toP(param)
  //    new SetFigureAction.SetFigureAction(if (command == Command.BUYARMY) Figure.Army else Figure.Scout, p)
//    }

    case Command.FORCEDMOVEFIGURES => new MoveAction.ForceMoveAction(toFigures(param))

    case Command.ENDOFPHASE => {

      val phase: TurnPhase.T = toTurnPhase(param)

      if (phase == TurnPhase.Research) new EndOfResearchAction
      else if (phase == TurnPhase.Trade) new EndOfTradeAction
      else
        new AbstractCommand(phase) {
          override def execute(board: GameBoard) = Unit

          override def verify(board: GameBoard): Mess = null
        }
    }

    case Command.STARTMOVE => new MoveAction.StartMoveAction(toFigures(param))

    case Command.MOVE => new MoveAction.MoveAction(toFiguresNull(param))

    case Command.ENDOFMOVE => new MoveAction.EndOfMoveAction(toFiguresNull(param))

    case Command.REVEALTILE => new RevealTileAction(toOrientation(param))

    case Command.EXPLOREHUT => new ExploreHutCommand.ExploreHutCommand()

    case Command.ATTACK => new AttackCommand.AttackCommand

    case Command.STARTBATTLE => new AttackCommand.StartBattleCommand(param)

    case Command.PLAYUNIT => new AttackCommand.PlayUnitCommand(false)

    case Command.PLAYUNITIRON => new AttackCommand.PlayUnitCommand(true)

    case Command.ENDBATTLE => new AttackCommand.EndOfBattleCommand(param)

    case Command.TAKEWINNERLOOT => new AttackCommand.TakeWinnerLootCommand(param)

    case _ => throw FatalError(Mess(M.NOTIMPLEMENTEDYET, command))
  }

  trait Command {

    // TODO: review
    // var only because of constructCommand, consider better solution
    // immutable outside
    var command: Command.T = _

    var civ: Civilization.T = _

    private var canceled: Boolean = false
    var p: P = _
    var j: JsValue = _

    def setCanceled = canceled = true

    def param: Any

    def param1: Any

    private var replay: Boolean = false

    def setReplay = replay = true

    def isReplay: Boolean = replay

    def isExecute: Boolean = !replay

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

    def commandsAvail(b: GameBoard, civ: Civilization.T,phase: TurnPhase.T): Seq[Command.T] =
      getSet.
        filter(Command.actionPhase(_) == phase).
        filter(!itemize(b, civ, _).isEmpty) toSeq

    def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = itemizeP(b, civ, com)

    // can be called only by itemize
    protected def itemizeP(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[CommandParams] =
      itemizePP(b, civ, com).map(p => CommandParams(Some(p), None))

    protected def itemizePP(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[P] = ???

    protected def emptyCommandPoint(param: JsValue): Command =
      new AbstractCommand(toP(param)) {
        override def execute(board: GameBoard) = Unit

        override def verify(board: GameBoard): Mess = null
      }

    protected def emptyCommand(): Command =
      new AbstractCommandNone() {
        override def execute(board: GameBoard) = Unit

        override def verify(board: GameBoard): Mess = null
      }


    def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command

    protected def defaultverify(board: GameBoard, civ: Civilization.T, com: Command.T, p: P, j: JsValue): Mess = {
      val itemi: Seq[JsValue] = itemize(board, civ, com)
      val par: CommandParams = CommandParams(if (p == null || p.empty) None else Some(p), if (j == null || j == JsNull) None else Some(j))
      if (itemi.find(eqJsParam(_, par)).isDefined) null else Mess(M.COMMANDPARAMETERDOESNOTMATCH, (civ, com, p, j))
    }

  }

}
