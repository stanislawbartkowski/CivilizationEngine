package civilization.action

import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.helper._
import civilization.objects.{TurnPhase, _}
import play.api.libs.json.JsValue

object CommandContainer {

  val commands: Seq[CommandPackage] = Seq(BuyUnit, SpendTrade, SendProduction, HarvestResource, ResearchTechnology,
    BuyBuildingCommand, BuyWorldWonder, BuildCityWalls, IncreaseTradeProductionAction,
    PotteryPhilosophyAction, SetCityAction, TakeResourceCommand, SpendSilkAction,
    SetFigureAction, DevoutToCultureCommand, AdvanceCulture, CurrencyAction, DiscardCard, GreatPersonAction,
    ConstructionAction, GetCultureAction, MetalCastingAction, BankingAction, ChivalryAction, GetResourceCommand,
    GetHutVillageCommand, GetCoinCommand, DestroyCityAction)

  val comset: Map[Command.T, CommandPackage] = commands.map(c => c.getSet.map(co => (co, c))).flatten.map(c => c._1 -> c._2) toMap

  def isCommandCovered(com: Command.T): Boolean = comset.contains(com)

  def commandsAvail(b: GameBoard, deck: PlayerDeck, phase: TurnPhase.T): Seq[Command.T] = {
    // it is necessary to have additional filter for phase, not all commands are passing through CommandPackage
    val co: Seq[Command.T] = commands.map(co => co.commandsAvail(b, deck, phase).filter(p => Command.actionPhase(p) == phase)).flatten.filter(!Command.internalAction(_))
    // technology resource command used already
    val techResourceUsed = technologyResourceUsed(b, deck)
    // if yes, then weed out all technology commands here
    if (!techResourceUsed) co else co.filter(!Command.isTechnologyResourceAction((_)))
  }

  def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] =
    comset.get(com).get.itemize(b, deck, com)

  def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = comset.get(command).get.produceCommand(command, civ, p, param)

}
