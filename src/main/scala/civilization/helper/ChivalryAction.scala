package civilization.helper

import civilization.action._
import civilization.gameboard
import civilization.objects._
import play.api.libs.json.{JsNumber, JsValue}

object ChivalryAction extends TechnologyResourceTrait {

  override val command: Command.T = Command.CHIVALRYACTION
//  override val tech: TechnologyName.T = TechnologyName.Chivalry

  protected class BankingAction(override val param: HVResource) extends TechnologyResourceAction(param) {

    override def executeI(board: gameboard.GameBoard): Unit = {
      // increase production by 5
      if (isExecute) board.addForcedCommandC(Command.GETCULTURE, civ, p, JsNumber(5))
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new BankingAction(param)

}
