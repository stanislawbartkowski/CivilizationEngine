package civilization.action

import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.objects.{TurnPhase, _}
import play.api.libs.json.JsValue

object CommandContainer {

  val commands: Seq[CommandPackage] = Seq(BuyUnit, SpendTrade, SendProduction, HarvestResource, ResearchTechnology, BuyBuildingCommand,BuyWorldWonder,PotteryPhilosophyAction)

  val comset: Map[Command.T, CommandPackage] = commands.map(c => c.getSet.map(co => (co, c))).flatten.map(c => c._1 -> c._2) toMap

  def isCommandCovered(com: Command.T): Boolean = comset.contains(com)

  def commandsAvail(b: GameBoard, civ: Civilization.T, phase: TurnPhase.T): Seq[Command.T] = {
    val co: Seq[Command.T] = commands.map(co => co.commandsAvail(b, civ).filter(p => Command.actionPhase(p) == phase)).flatten
    // technology resource command used already
    val techResourceUsed = technologyResourceUsed(b, civ)
    // if yes, then weed out all technology commands here
    if (!techResourceUsed) co else co.filter(!Command.isTechnologyResourceAction((_)))
  }

  def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] =
    comset.get(com).get.itemize(b, civ, com)

  def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = comset.get(command).get.produceCommand(command, civ, p, param)

}
