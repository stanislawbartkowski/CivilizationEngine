package civilization.helper

import civilization.gameboard
import civilization.gameboard._
import civilization.message.{M, Mess}
import civilization.objects._

object PrintingPressAction extends CoinActionTrait {

  override val command: Command.T = Command.PRINTINGPRESSACTION
  override val tech: TechnologyName.T = TechnologyName.PrintingPress

  override def validateH(b: GameBoard, deck: PlayerDeck, command: Command.T): Option[Mess] = {
    val cult = deck.resou.nof(Resource.Culture)
    if (cult <= 5)
      Some(Mess(M.NOTENOUGHVULTURETOSPEND, (command, cult)))
    else None
  }

  override def executeI(board: gameboard.GameBoard, deck : PlayerDeck): Unit =
    board.increaseCultureCommand(deck,-5)

}
