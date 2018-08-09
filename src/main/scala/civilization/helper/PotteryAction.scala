package civilization.helper
import civilization.gameboard.PlayerTechnology
import civilization.{gameboard, message}
import civilization.objects._

object PotteryAction extends TechnologyAnyResourceAction {
  override val command: Command.T = Command.POTTERYACTION
  override val tech: TechnologyName.T = TechnologyName.Pottery

  override def validateH(b: gameboard.GameBoard, deck: gameboard.PlayerDeck, command: Command.T): Option[message.Mess] = None

  override def executeI(board: gameboard.GameBoard, deck: gameboard.PlayerDeck, isExecute : Boolean): Unit = {
    val te: PlayerTechnology = deck.findPlayerTechnology(tech).get
    addCoinToTechnology(board, te)
  }

}
