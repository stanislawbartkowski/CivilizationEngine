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
    val cost: Int = ObjectCost.getCost(u, limit.playerStrength)
    CitiesCanAfford(b, civ, cost)
  }

  override def commandsAvail(b: gameboard.GameBoard, civ: Civilization.T): Seq[Command.T] = {
    val limit: PlayerLimits = getLimits(b, civ)
    (if (itemizeI(b, civ, Command.BUYARTILLERY, limit).isEmpty) Nil else Seq(Command.BUYARTILLERY)) ++
      (if (itemizeI(b, civ, Command.BUYARTILLERY, limit).isEmpty) Nil else Seq(Command.BUYARTILLERY)) ++
      (if (itemizeI(b, civ, Command.BUYARTILLERY, limit).isEmpty) Nil else Seq(Command.BUYARTILLERY)) ++
      (if (itemizeI(b, civ, Command.BUYARTILLERY, limit).isEmpty) Nil else Seq(Command.BUYARTILLERY))
  }

  override def itemize(b: gameboard.GameBoard, civ: Civilization.T, com: Command.T): JsArray = {
    val limit: PlayerLimits = getLimits(b, civ)
    itemizeI(b, civ, com, limit)
  }

  class BuyUnitAction(override val param: P) extends AbstractCommand(param) {
    def execute(board: GameBoard) = ???

    def verify(board: GameBoard): Mess = ???
  }


  override def produceCommand(par: JsValue) =
    new BuyUnitAction(par)
}
