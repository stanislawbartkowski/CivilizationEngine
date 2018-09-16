package civilization.helper

import civilization.action._
import civilization.gameboard
import civilization.objects.{Command, _}
import play.api.libs.json.{JsNumber, JsValue}

object MetalCastingAction extends TechnologyResourceTrait {

  override val command: Command.T = Command.METALCASTINGACTION

  protected class MetalCastingAction(override val param: HVResource) extends TechnologyResourceAction(param) {

    override def executeI(board: gameboard.GameBoard): Unit = {
      // increase culture by 7
      if (isExecute) board.increaseCultureCommand(civ, 7)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new MetalCastingAction(param)

}
