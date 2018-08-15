package civilization.action

import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.helper._
import civilization.objects.{TurnPhase, _}
import play.api.libs.json.JsValue

object CommandContainer {

  val commands: Seq[CommandPackage] = Seq(BuyUnit, SpendTrade, SendProduction, HarvestResource, ResearchTechnology,
    BuyBuildingCommand, BuyWorldWonder, BuildCityWalls, IncreaseTradeProductionAction,
    PhilosophyAction, PotteryAction, SetCityAction, TakeResourceCommand, SpendSilkAction,
    SetFigureAction, DevoutToCultureCommand, AdvanceCulture, CurrencyAction, DiscardCard, GreatPersonAction,
    ConstructionAction, GetCultureAction, MetalCastingAction, BankingAction, ChivalryAction, GetResourceCommand,
    GetHutVillageCommand, GetCoinCommand, DestroyCityAction, WinTheGame, DemocracyAction, PrintingPressAction)

  val comset: Map[Command.T, CommandPackage] = commands.map(c => c.getSet.map(co => (co, c))).flatten.map(c => c._1 -> c._2) toMap

  def isCommandCovered(com: Command.T): Boolean = comset.contains(com)

  def commandsAvail(b: GameBoard, deck: PlayerDeck, phase: TurnPhase.T): Seq[Command.T] = {
    // it is necessary to have additional filter for phase, not all commands are passing through CommandPackage
    val co: Seq[Command.T] = commands.map(co => co.commandsAvail(b, deck, phase).filter(Command.inPhase(_, phase))).flatten.filter(!Command.internalAction(_))
    co
  }

  def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] =
    comset.get(com).get.itemize(b, deck, com)

  def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = comset.get(command).get.produceCommand(command, civ, p, param)

}
