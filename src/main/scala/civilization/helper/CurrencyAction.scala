package civilization.helper

import civilization.action._
import civilization.helper.PotteryPhilosophyAction.PotteryPhilosophyAction
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.{action, gameboard, message, objects}
import civilization.objects.Command
import civilization.objects.Command.T
import civilization.objects.Resource.T
import civilization.objects.TechnologyName.T
import play.api.libs.json.JsValue
import civilization.objects._

object CurrencyAction extends TechnologyResourceTrait {

  override val command: Command.T = Command.CURRENCYACTION
  override val tech: TechnologyName.T = TechnologyName.Currency

  protected class CurrencyAction(override val param: HVResource) extends TechnologyResourceAction(param)  {

    override def executeI(board: gameboard.GameBoard): Unit = {
      // increase culture by 3
      if (isExecute) board.addForcedCommandC(Command.GET3CULTURE,civ)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new CurrencyAction(param)

}
