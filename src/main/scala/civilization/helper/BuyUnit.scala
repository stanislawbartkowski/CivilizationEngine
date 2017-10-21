package civilization.helper

import civilization.action._
import civilization.gameboard
import civilization.gameboard.{Figures, GameBoard}
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

  override def getSet: Set[Command.T] = Set(Command.BUYARTILLERY, Command.BUYAIRCRAFT, Command.BUYMOUNTED, Command.BUYINFANTRY)

  private def itemizeI(b: gameboard.GameBoard, civ: Civilization.T, com: Command.T, limit: PlayerLimits): Seq[P] = {
    val u: CombatUnitType.T = toU(com)
    if (u == CombatUnitType.Aircraft && !limit.aircraftUnlocked) return Nil
    val cost: Int = ObjectCost.getCost(u, limit.playerStrength.getStrength(u))
    CitiesCanAfford(b, civ, cost)
  }

  override def itemizeP(b: gameboard.GameBoard, civ: Civilization.T, com: Command.T): Seq[CommandParams] = {
    val limit: PlayerLimits = getLimits(b, civ)
    val cities : Seq[P] = itemizeI(b, civ, com, limit)
    cities.map(p => CommandParams(Some(p),None))
  }

  class BuyUnitAction extends AbstractCommandNone {
    def execute(board: GameBoard) = {
      val u: CombatUnitType.T = toU(command)
      val co: CombatUnit = getRandomUnit(board, u)
      board.playerDeck(civ).units = board.playerDeck(civ).units :+ co
    }

    def verify(board: GameBoard): Mess = defaultverify(board, civ, command, p, j)
  }


  override def produceCommand(par: JsValue) =
    new BuyUnitAction()
}
