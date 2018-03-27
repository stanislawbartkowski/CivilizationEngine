package civilization.io.fromjson

import civilization.gameboard.{EnumResources, _}
import civilization.helper._
import civilization.objects._
import play.api.libs.json._

trait ImplicitMiximFromJson {

  implicit def toPoint(j: JsValue): P = convert[PJ](PJ(j))

  implicit def toInt(j: JsValue): Int = j.as[Int]


  implicit val gameBuildingsReads: Reads[BuildingsResources] = new Reads[BuildingsResources] {
    def reads(json: JsValue): JsResult[BuildingsResources] = {
      val r: BuildingsResources = new BuildingsResources()
      val a: JsArray = json.as[JsArray]
      a.value.foreach(i => {
        val j: JsValue = i
        val re: BuildingName.T = (i \ S.name).get.as[BuildingName.T]
        val num: Int = (i \ S.num).get.as[Int]
        r.setResNum(re, num)
      })
      JsSuccess(r)
    }
  }

  implicit val gameResourcesReads: Reads[BoardResources] = new Reads[BoardResources] {
    def reads(json: JsValue): JsResult[BoardResources] = {
      val r: BoardResources = new BoardResources()
      val a: JsArray = json.as[JsArray]
      a.value.foreach(i => {
        val j: JsValue = i
        val re: Resource.T = (i \ S.resource).get.as[Resource.T]
        val num: Int = (i \ S.num).get.as[Int]
        r.setResNum(re, num)
      })
      JsSuccess(r)
    }
  }

  implicit def toOrientation(j: JsValue): Orientation.T = convert[OrientationJ](OrientationJ(j))

  implicit def toBattleStart(j: JsValue): BattleStart = j.as[BattleStart]

  implicit def toCombatUnit(j: JsValue): CombatUnit = convert[CombatUnitJ](CombatUnitJ(j))

  implicit def toWinnerLootNull(j: JsValue): WinnerLoot =
    if (j == null || j == JsNull) WinnerLoot(None, None, false, false) else j.as[WinnerLoot]

  implicit def toTakeWinnerLoot(j: JsValue): TakeWinnerLoot = j.as[TakeWinnerLoot]

  implicit def toTechnologName(j: JsValue) = j.as[TechnologyName.T]

  implicit def toBuildingPoint(j : JsValue) : BuildingPoint = j.as[BuildingPoint]

  implicit def toHutVillageT(j : JsValue) : HutVillage.T = j.as[HutVillage.T]

  implicit def toHVResource(j : JsValue) : HVResource = j.as[HVResource]

  implicit def toHVResourceSeq(j : JsValue) : Seq[HVResource] = j.as[Seq[HVResource]]

}
