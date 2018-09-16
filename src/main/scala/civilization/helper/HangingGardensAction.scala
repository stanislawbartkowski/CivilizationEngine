package civilization.helper

import civilization.message.{M, Mess}
import civilization.action.{AbstractCommand, AbstractCommandNone, CommandPackage}
import civilization.gameboard.{Figures, GameBoard, MapSquare, PlayerDeck}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, P, _}
import civilization.{gameboard, message}
import play.api.libs.json.JsValue


object HangingGardensAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.FREEARMY, Command.FREESCOUT)

  private def toF(com: Command.T): Figure.T =
    if (com == Command.FREESCOUT) Figure.Scout else Figure.Army

  private def featureavailable(b: GameBoard, deck: PlayerDeck, comm: Command.T): Option[Mess] = {
    if (!hasWonderFeature(b, deck, WonderFeatures.freeFiguresAtTheStartOfTurn)) Some(Mess(M.HANGINGGARDENWONDERNOTAVAILABLE, (deck.civ, comm)))
    else if (firstRound(b, Some(TurnPhase.StartOfTurn))) Some(Mess(M.CANNOTGETFREEFIGUREHANGINGGARDENINFIRSTTURN, (deck.civ, comm)))
    else if (commandUsedAlready(b, deck.civ, TurnPhase.StartOfTurn, Command.FREESCOUT) || commandUsedAlready(b, deck.civ, TurnPhase.StartOfTurn, Command.FREEARMY))
      return Some(Mess(M.COMMANDUSEDALREADYINTHISTURN, (deck.civ, comm)))
    else {
      val li: PlayerLimits = getLimits(b, deck)
      val m: Option[Mess] = figureAvailable(b, deck, toF(comm), li)
      if (!m.isDefined) m
      else {
        val ma: MapSquareP = findWonder(b, deck, WonderFeatures.freeFiguresAtTheStartOfTurn).get
        val m: Option[Mess] = isSquareForFigures(b, deck, new Figures(toF(comm)), ma.s, li)
        m
      }
    }
  }

  protected class FreeFigure extends AbstractCommandNone {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val m: Option[Mess] = featureavailable(board, deck, command)
      if (m.isDefined) m.get
      else null
    }

    override protected def execute(board: GameBoard): Unit = {
      val ma: MapSquareP = findWonder(board, deck, WonderFeatures.freeFiguresAtTheStartOfTurn).get
      putFigure(board, civ, ma.p, toF(command))
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new FreeFigure

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] =
    if (featureavailable(b, deck, com).isEmpty) emptyItemize() else Nil

}
