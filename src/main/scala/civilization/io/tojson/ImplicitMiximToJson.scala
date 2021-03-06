package civilization.io.tojson

import civilization.gameboard._
import civilization.io.fromjson.{TurnPhaseJ, convert}
import civilization.objects._
import play.api.libs.json._
import civilization.io.readdir.Param._


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

//  implicit def writeTakeWinnerLoot(m: TakeWinnerLoot): JsValue = Json.toJson(m)

  implicit val buildingpointWrites: Writes[BuildingPoint] = new Writes[BuildingPoint] {
    override def writes(o: BuildingPoint): JsValue = Json.obj(
      S.p -> o.p,
      S.building -> o.b
    )
  }

  implicit val hvResourceWrites: Writes[HVResource] = new Writes[HVResource] {
    override def writes(o: HVResource): JsValue = Json.obj(
      S.hutvillage -> o.hv,
      S.resource -> o.resource
    )
  }

  implicit val endofGameWrites : Writes[EndOfGame] = new Writes[EndOfGame] {
    override def writes(o: EndOfGame): JsValue = Json.obj(
      S.winner -> o.winner,
      "wintype" -> o.wintype
    )
  }

  implicit val hvResourceCivWrites: Writes[HVResourcesForCiv] = new Writes[HVResourcesForCiv] {
    override def writes(o: HVResourcesForCiv): JsValue = Json.obj(
      S.resource -> o.resource,
      S.civ -> o.civ
    )
  }

  implicit def writeListOfHVResourceCiv(p : Seq[HVResourcesForCiv]) : Seq[JsValue] =
    p.map(Json.toJson(_))

  implicit def writePP(a: Seq[(P, P)]): Seq[JsValue] = a.map(writesCityPoint)

  implicit def writePSeqP(a: Seq[(P, Seq[P])]): Seq[JsValue] = a.map(e => {
    Json.obj(
      S.p -> e._1,
      S.list -> e._2
    )
  }
  )

  implicit val suspendedActionWrite: Writes[ActionTypeSuspension] = new Writes[ActionTypeSuspension] {
    override def writes(o: ActionTypeSuspension): JsValue = Json.obj(
      S.civ -> o.civ,
      S.command -> o.comm,
      S.param -> o.par
    )
  }

  implicit def writeCultureCost(l: Seq[CultureTrack.CultureTrackCost]): Seq[JsValue] = l.map(writeCultureTrackCost)

  implicit def writeGreatPerson(p : GreatPersonName.T) : JsValue = Json.toJson(p)
  implicit def writeCultureCard(p : CultureCardName.T) : JsValue = Json.toJson(p)

  implicit def writeListCultureCard(p : Seq[CultureCardName.T]) : Seq[JsValue] = p.map(writeCultureCard)

  implicit def writeListGreatPersonName(p : Seq[GreatPersonName.T]) : Seq[JsValue] = p.map(writeGreatPerson)

  implicit def writesWonder(w : Wonders.T) : JsValue = Json.toJson(w)

  implicit def writeResource(r : Resource.T) : JsValue = Json.toJson(r)

  implicit def writeListOfResources(p : Seq[Resource.T]) : Seq[JsValue] = p.map(writeResource)

  implicit def toTechnologyName(t : TechnologyName.T) : JsValue = Json.toJson(t)

  implicit def writeListOfTechnlogyNames(p : Seq[TechnologyName.T]) : Seq[JsValue] = p.map(toTechnologyName)

  implicit def toHutVillage(h: HutVillage): JsValue = Json.toJson(h)

  implicit def WriteGameWinType(g : GameWinType.T) : JsValue = Json.toJson(g)

  implicit def toTurnPhase(j: JsValue): TurnPhase.T = convert[TurnPhaseJ](TurnPhaseJ(j))

}
