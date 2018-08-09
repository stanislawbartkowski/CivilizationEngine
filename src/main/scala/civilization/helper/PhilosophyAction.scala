package civilization.helper

import civilization.gameboard.PlayerTechnology
import civilization.objects._
import civilization.{gameboard, message}

object PhilosophyAction extends TechnologyAnyResourceAction {
  override val command: Command.T = Command.PHILOSOPHYACTION
  override val tech: TechnologyName.T = TechnologyName.Philosophy

  override def validateH(b: gameboard.GameBoard, deck: gameboard.PlayerDeck, command: Command.T): Option[message.Mess] = None

  override def executeI(board: gameboard.GameBoard, deck: gameboard.PlayerDeck, isExecute: Boolean): Unit =
    if (isExecute) board.addForcedCommandC(Command.GREATPERSON, deck, null, getRandomPerson(board))
}
