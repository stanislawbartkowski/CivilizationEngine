package civilization

import civilization.action.Command
import civilization.gameboard._
import civilization.helper._
import civilization.helper.battle.AttackCommand
import civilization.helper.move.{ExploreHutCommand, MoveAction, RevealTileAction}
import civilization.io.fromjson._
import civilization.io.tojson._
import civilization.message.{FatalError, M, Mess, J}
import civilization.objects.Resource.Value
import civilization.objects._
import play.api.libs.json.{JsNull, JsValue}

package object action extends ImplicitMiximToJson with ImplicitMiximFromJson {


  private def enrich(c: Command, command: Command.T, civ: Civilization.T, p: P, status: CommandStatus.T, param: JsValue): Command = {
    c.command = command
    c.p = p
    c.civ = civ
    c.j = param
    c.setStatus(status)
    c
  }

  def constructCommand(c: CommandValues): Command =
    constructCommand(c.command, c.civ, c.p, c.status, c.param)

  def constructCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue = null): Command =
    constructCommand(command, civ, p, CommandStatus.No, param)

  def constructCommand(command: Command.T, civ: Civilization.T, p: P, status: CommandStatus.T, param: JsValue): Command = {
    assert(civ != null && command != null)
    val c: Command = if (CommandContainer.isCommandCovered(command)) CommandContainer.produceCommand(command, civ, p, param) else produceCommand(command, civ, p, param)
    enrich(c, command, civ, p, status, param)
  }

  private def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = command match {


    case Command.FORCEDMOVEFIGURES => new MoveAction.ForceMoveAction(toFigures(param))

    case Command.ENDOFPHASE => {

      val phase: TurnPhase.T = toTurnPhase(param)

      phase match {
        case TurnPhase.Research => new EndOfResearchAction
        case TurnPhase.Trade => new EndOfTradeAction
        case _ => new EndOfPhaseAbstract(param) {
          override protected def executeC(board: GameBoard): Unit = {}
        }
      }

      /*
      if (phase == TurnPhase.Research) new EndOfResearchAction
      else if (phase == TurnPhase.Trade) new EndOfTradeAction
      else if (phase == TurnPhase.Movement) new EndOfMovementAction
      else
        new AbstractCommand(phase) {
          override def execute(board: GameBoard) = Unit
        }
        */
    }

    case Command.STARTMOVE => new MoveAction.StartMoveAction(toFigures(param))

    case Command.SACRIFICEFIGUREFORTECH => new MoveAction.SacrificeFigureFortech(param)

    case Command.MOVE => new MoveAction.MoveAction(toFiguresNull(param))

    case Command.ENDOFMOVE => new MoveAction.EndOfMoveAction(toFiguresNull(param))

    case Command.KILLFIGURE => new MoveAction.KillFigureAction(toFiguresNull(param))

    case Command.REVEALTILE => new RevealTileAction(toOrientation(param))

    case Command.EXPLOREHUT => new ExploreHutCommand.ExploreHutCommand()

    case Command.ATTACK => new AttackCommand.AttackCommand

    case Command.STARTBATTLE => new AttackCommand.StartBattleCommand(param)

    case Command.PLAYUNIT => new AttackCommand.PlayUnitCommand(false)

    case Command.PLAYUNITIRON => new AttackCommand.PlayUnitCommand(true)

    case Command.ENDBATTLE => new AttackCommand.EndOfBattleCommand(param)

    case Command.SAVEUNIT => new AttackCommand.SaveUnitCommand

    case _ => throw FatalError(Mess(M.NOTIMPLEMENTEDYET, command))
  }

  trait Command {

    // TODO: review
    // var only because of constructCommand, consider better solution
    // immutable outside
    var command: Command.T = _

    var civ: Civilization.T = _

    protected var deck: PlayerDeck = _

    var p: P = _
    var j: JsValue = _

    def param: Any

    def param1: Any

    private var _replay: Boolean = false

    def setReplay = _replay = true

    def setNoReplay: Unit = _replay = false

    def isExecute: Boolean = !_replay

    private var _status: CommandStatus.T = CommandStatus.No

    def status: CommandStatus.T = _status

    def setStatus(sta: CommandStatus.T) = _status = sta

    def setSuspended = _status = CommandStatus.Su

    def isSuspended: Boolean = _status == CommandStatus.Su

    def setCanceled = _status = CommandStatus.Ca

    def isCanceled: Boolean = _status == CommandStatus.Ca

    def setNormal: Unit = _status = CommandStatus.No

    def isNormal: Boolean = _status == CommandStatus.No

    private var _executesuspendedcommand: Boolean = false

    def setExecuteSuspendedCommand = _executesuspendedcommand = true

    def isExecuteSuspendedCommand: Boolean = _executesuspendedcommand

    private var _cancelsuspendedcommand: Boolean = false

    protected def setCancelSuspendedCommand = _cancelsuspendedcommand = true

    def isCancelSuspendedCommand: Boolean = _cancelsuspendedcommand

    private var _forgetCurrentCommand: Boolean = false

    protected def setForgetCurrentCommand = _forgetCurrentCommand = true

    def isForgetCurrentCommand: Boolean = _forgetCurrentCommand


    // TODO: should return Option[Mess], not null
    // null : success, Mess : failure and failure info

    def verifyCommand(board: GameBoard): Mess = {
      deck = board.playerDeck(civ)
      verify(board)
    }


    protected def registerCommandInJournal(board: GameBoard) = registerCommandInJournalDefault(board)

    protected def registerCommandInJournalDefault(board: GameBoard) = {
      val op : Option[P] = if (p != null) Some(p) else None
      val ojparam : Option[JsValue] = if (j != null) Some(j) else None
      val opa : Option[CommandParams] = if (op == None && ojparam == None) None else Some(CommandParams(op,ojparam))
      addToJournal(board, civ, isExecute, J.DOACTION, List(command.toString()), None, opa)
    }

    protected def verify(board: GameBoard): Mess = null

    def executeCommand(board: GameBoard) = {
      if (isExecute) registerCommandInJournal(board)
      deck = board.playerDeck(civ)
      execute(board)
    }

    protected def execute(board: GameBoard)
  }

  abstract class AbstractCommandNone(val param: Any = null, var param1: Any = null) extends Command

  abstract class AbstractCommand[T](val param: T = null, val param1: Any = null) extends Command

  abstract class AbstractCommand1[T, T1](val param: T = null, var param1: T1 = null) extends Command

  trait CommandPackage {
    def getSet: Set[Command.T]

    def commandsAvail(b: GameBoard, deck: PlayerDeck, phase: TurnPhase.T): Seq[Command.T] =
      getSet.
        filter(Command.inPhase(_, phase)).
        filter(!itemize(b, deck, _).isEmpty) toSeq

    def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = itemizeP(b, deck, com)

    // can be called only by itemize
    protected def itemizeP(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[CommandParams] =
      itemizePP(b, deck, com).map(p => CommandParams(Some(p), None))

    protected def itemizePP(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[P] = ???

    protected def emptyCommandPoint(param: JsValue): Command =
      new AbstractCommand(toP(param)) {
        override def execute(board: GameBoard) = Unit
      }

    protected def emptyCommand(): Command =
      new AbstractCommandNone() {
        override def execute(board: GameBoard) = Unit
      }

    protected def emptyItemizeP(): Seq[P] = Seq(P(0, 0))

    protected def emptyItemize(): Seq[JsValue] = emptyItemizeP()

    def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command

    protected def defaultverify(board: GameBoard, deck: PlayerDeck, com: Command.T, p: P, j: JsValue): Mess = {
      val itemi: Seq[JsValue] = itemize(board, deck, com)
      val par: CommandParams = CommandParams(if (p == null || p.empty) None else Some(p), if (j == null || j == JsNull) None else Some(j))
      if (itemi.find(eqJsParam(_, par)).isDefined) null else Mess(M.COMMANDPARAMETERDOESNOTMATCH, (deck.civ, com, p, j))
    }
  }

  trait SuspendCommandTrait extends CommandPackage {
    def canSuspend(board: GameBoard, deck: PlayerDeck, comm: Command)
  }

  def toC(com: Command): CommandValues = CommandValues(com.command, com.civ, com.p, com.status, com.j)

}
