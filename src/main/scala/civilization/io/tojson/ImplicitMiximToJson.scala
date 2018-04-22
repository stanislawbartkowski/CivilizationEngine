package civilization.io.tojson

import civilization.gameboard._
import civilization.objects._
import play.api.libs.json.{JsArray, JsValue, Json, Writes}

trait ImplicitMiximToJson {

  implicit def writesPoint(p: P): JsValue = Json.toJson(p)

  implicit def toSeqP(li: Seq[P]): Seq[JsValue] =
    li.map(writesPoint)

  implicit def CPtoJS(c: CommandParams): JsValue = {
    if (c.p.isDefined)
      if (c.param.isDefined) Json.obj(
        S.p -> c.p,
        S.param -> c.param
      )
      else Json.obj(S.p -> c.p)
    // either p or param is defined, cannot be both None
    else Json.obj(S.param -> c.param)
  }

  implicit def toJSArrayParams(li: Seq[CommandParams]): Seq[JsValue] = li.map(CPtoJS)

  implicit def writesCityPoint(p: (P, P)): JsValue = Json.obj(
    S.p -> p._1,
    S.param -> p._2)

  implicit def toJSonStartParam(p: BattleStart): JsValue = Json.toJson(p)

  implicit def writesFigures(f: Figures) = Json.toJson(f)

  implicit def writeCombatUnit(m: CombatUnit): JsValue = Json.toJson(m)

  implicit def writeTakeWinnerLoot(m: TakeWinnerLoot): JsValue = Json.toJson(m)

  implicit val buildingpointWrites: Writes[BuildingPoint] = new Writes[BuildingPoint] {
    override def writes(o: BuildingPoint): JsValue = Json.obj(
      S.p -> o.p,
      S.building -> o.b
    )
  }

  implicit def writePP(a: Seq[(P, P)]): Seq[JsValue] = a.map(writesCityPoint(_))

  implicit def writePSeqP(a: Seq[(P, Seq[P])]): Seq[JsValue] = a.map(e => {
    Json.obj(
      S.p -> e._1,
      S.list -> e._2
    )
  }
  )

  implicit def writeCultureCost(l: Seq[CultureTrack.CultureTrackCost]): Seq[JsValue] = l.map(writeCultureTrackCost)

}
