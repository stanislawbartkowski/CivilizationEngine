package civilization.io.fromjson

import civilization.gameboard.{EnumResources, _}
import civilization.helper._
import civilization.objects._
import play.api.libs.json._
import civilization.io.readdir.Param._

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

  implicit def toTechnologName(j: JsValue) = j.as[TechnologyName.T]

  implicit def toLootEffects(j : JsValue) : Seq[WinnerLootEffect] = j.as[Seq[WinnerLootEffect]]

  implicit def toBuildingPoint(j : JsValue) : BuildingPoint = j.as[BuildingPoint]

  implicit def toHutVillageT(j : JsValue) : HutVillage.T = j.as[HutVillage.T]

  implicit def toHutVillage(j : JsValue) : HutVillage = j.as[HutVillage]

  implicit def toHVResource(j : JsValue) : HVResource = j.as[HVResource]

  implicit def toHVResourceSeq(j : JsValue) : Seq[HVResource] = j.as[Seq[HVResource]]

  implicit def toHVResourceDic(j : JsValue) : HVResourceCiv = j.as[HVResourceCiv]

  implicit def toSeqP(j : JsValue) : Seq[P] = j.as[Seq[P]]

  implicit def toSeqParams(j: JsValue): Seq[CommandValues] = j.as[Seq[CommandValues]]

  implicit def toCultureCard(j : JsValue) : CultureCardName.T = j.as[CultureCardName.T]

  implicit def toGreatPerson(j : JsValue) : GreatPersonName.T = j.as[GreatPersonName.T]

  implicit def toWorldWonder(j : JsValue) : Wonders.T = j.as[Wonders.T]

  implicit def toResource(j : JsValue) : Resource.T = j.as[Resource.T]

  implicit def toGameWinType(j : JsValue) : GameWinType.T = j.as[GameWinType.T]

  implicit def toParams(j: JsValue): CommandValues = j.as[CommandValues]

}
