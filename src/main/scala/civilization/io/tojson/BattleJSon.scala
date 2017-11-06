package civilization.io.tojson

import civilization.gameboard._
import civilization.helper._
import civilization.objects._
import play.api.libs.json._
import civilization.io.tojson.CombatUnitJSon.unitstoJSON


object BattleJSon {

  implicit val fronUnit: Writes[FrontUnit] = new Writes[FrontUnit] {
    override def writes(f: FrontUnit) = Json.obj(
      "wounds" -> f.wounds,
      "attachstrength" -> f.attackstrength,
      "defendstrength" -> f.defendstrenght,
      "unit" -> f.unit
    )
  }

  implicit val battleArmy: Writes[BattleArmy] = new Writes[BattleArmy] {
    override def writes(o: BattleArmy) = Json.toJson(o.map(u => Json.toJson(u)))
  }


  def genbattlefileSide(b: BattleFieldSide, details: Boolean ) = Json.obj(
    S.combatbonus -> b.combatBonus,
    "front" -> b.fighting,
    "canuseiron" -> b.canuseiron,
    "ironused" -> b.ironused,
    S.killedunits -> b.killed,
    "waiting" -> unitstoJSON(b.waiting,details,b.strength),
    S.you -> details
  )

  def genBattleJson(g: GameBoard, civ : Civilization.T): JsValue =
    if (g.battle.isEmpty) JsNull
    else {

      Json.obj(
        "atackerturn" -> g.battle.get.attackermove,
        "endofbattle" -> g.battle.get.endofbattle,
        S.attacker -> genbattlefileSide(g.battle.get.attacker,civ == g.battle.get.attackerciv),
        S.defender -> genbattlefileSide(g.battle.get.defender,civ == g.battle.get.defenderciv)
      )
    }

}
