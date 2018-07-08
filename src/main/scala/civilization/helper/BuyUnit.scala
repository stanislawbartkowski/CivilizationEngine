package civilization.helper

import civilization.action.{AbstractCommand, _}
import civilization.gameboard
import civilization.gameboard.{Figures, GameBoard, PlayerDeck}
import civilization.message.Mess
import civilization.objects._
import play.api.libs.json.{JsArray, JsValue}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson


object BuyUnit extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  private def toU(co: Command.T): CombatUnitType.T =
    co match {
      case Command.BUYINFANTRY => CombatUnitType.Infantry
      case Command.BUYMOUNTED => CombatUnitType.Mounted
      case Command.BUYAIRCRAFT => CombatUnitType.Aircraft
      case Command.BUYARTILLERY => CombatUnitType.Artillery
    }

  override def getSet: Set[Command.T] = Set(Command.BUYARTILLERY, Command.BUYAIRCRAFT, Command.BUYMOUNTED, Command.BUYINFANTRY, Command.TAKEUNIT)

  private def itemizeI(b: gameboard.GameBoard, civ: PlayerDeck, com: Command.T, limit: PlayerLimits): Seq[P] = {
    if (com == Command.TAKEUNIT) return Nil
    val u: CombatUnitType.T = toU(com)
    if (u == CombatUnitType.Aircraft && !limit.aircraftUnlocked) return Nil
    val cost: Int = ObjectCost.getCost(u, limit.playerStrength.getStrength(u))
    CitiesCanAfford(b, civ, cost)
  }

  override def itemizePP(b: gameboard.GameBoard, deck : PlayerDeck, com: Command.T): Seq[P] = {
    val limit: PlayerLimits = getLimits(b, deck)
    itemizeI(b, deck, com, limit)
  }

  protected class BuyUnitAction extends AbstractCommandNone {
    def execute(board: GameBoard) = {
      val u: CombatUnitType.T = toU(command)
      checkKilledUnits(board,u)
      if (isExecute) {
        val co: CombatUnit = getRandomUnit(board, u,false)
        //        board.playerDeck(civ).units = board.playerDeck(civ).units :+ co
        val commandC: Command = constructCommand(Command.TAKEUNIT, civ, p, co)
        // execute later
        board.addForcedCommand(commandC)
      }
    }

    def verify(board: GameBoard): Mess = defaultverify(board, deck, command, p, j)
  }

  protected class TakeUnitAction(override val param: CombatUnit) extends AbstractCommand(param) {

    override def verify(board: GameBoard): Mess = null

    override def execute(board: GameBoard): Unit = {
      deck.units = deck.units :+ param
      val fun = (p1: CombatUnit,p2 : CombatUnit ) => { p1 == p2 }
      board.market.units = removeElem(board.market.units,param,(p1: CombatUnit,p2 : CombatUnit ) => { p1 == p2 })
    }
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) =
    command match {
      case Command.TAKEUNIT => new TakeUnitAction(param)
      case _ => new BuyUnitAction()
    }
}
