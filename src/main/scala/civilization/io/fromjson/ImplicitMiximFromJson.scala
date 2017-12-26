package civilization.io.fromjson

import civilization.gameboard.{Figures, GameResources}
import civilization.objects._
import play.api.libs.json._

trait ImplicitMiximFromJson {

  implicit def toPoint(j: JsValue): P = convert[PJ](PJ(j))

  implicit def toInt(j : JsValue) : Int = j.as[Int]

  implicit val gameResourcesReads : Reads[GameResources] = new Reads[GameResources] {
    def reads(json: JsValue): JsResult[GameResources] = {
      val r : GameResources = new GameResources()
      val a : JsArray = json.as[JsArray]
      a.value.foreach( i => {
        val j : JsValue = i
        val re : Resource.T = (i \ S.resource).get.as[Resource.T]
        val num : Int = (i \ S.num).get.as[Int]
        r.setResNum(re,num)
      })
      JsSuccess(r)
    }
  }

  implicit def toOrientation(j: JsValue): Orientation.T = convert[OrientationJ](OrientationJ(j))

  implicit def toBattleStart(j : JsValue) :BattleStart = j.as[BattleStart]

  implicit def toCombatUnit(j : JsValue) : CombatUnit = convert[CombatUnitJ](CombatUnitJ(j))

}
