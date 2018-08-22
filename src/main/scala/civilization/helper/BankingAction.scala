package civilization.helper

import civilization.action._
import civilization.gameboard
import civilization.objects._
import play.api.libs.json.{JsValue, JsNumber}

object BankingAction extends TechnologyResourceTrait {

  override val command: Command.T = Command.BANKINGACTION
//  override val tech: TechnologyName.T = TechnologyName.Banking

  protected class BankingAction(override val param: HVResource) extends TechnologyResourceAction(param) {

    override def executeI(board: gameboard.GameBoard): Unit = {
      // increase production by 5
      if (isExecute) board.addForcedCommandC(Command.INCREASEPRODUCTION, civ, p, JsNumber(7))
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new BankingAction(param)

}
