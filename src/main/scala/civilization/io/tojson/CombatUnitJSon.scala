package civilization.io.tojson

import civilization.objects.{CombatUnit, CombatUnitStrength, CombatUnitType, S}
import play.api.libs.json.{JsValue, Json}

object CombatUnitJSon {

  def unitstoJSON(li: Seq[CombatUnit], detail: Boolean, s: CombatUnitStrength): JsValue = {
    val utype: Map[CombatUnitType.T, Seq[CombatUnit]] = li.groupBy(_.utype)
    //    val ulist : Seq[(CombatUnitType.T,Int)] = utype.map(u => (u._1,u._2.length)
    val ulist: Seq[(CombatUnitType.T, Int)] = CombatUnitType.values.map(u => (u, if (utype.get(u).isDefined) utype.get(u).get.length else 0)) toSeq
    val useq: Seq[JsValue] = ulist.map(j =>
      if (s == null) Json.obj(
        S.unitname -> j._1,
        S.num -> j._2
      ) else
        Json.obj(
          S.unitname -> j._1,
          S.num -> j._2,
          "militarystrength" -> s.getStrength(j._1)
        )
    )

    if (detail)
      Json.obj(
        S.units -> useq,
        S.list -> li
      )
    else
      Json.obj(
        S.units -> useq
      )
  }


}
