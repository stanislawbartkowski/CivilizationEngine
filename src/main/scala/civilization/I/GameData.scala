package civilization.I

import civilization.objects._
import play.api.libs.json.{JsValue, Json}

case class GameData(val gameid : Int, val civ: Seq[Civilization.T], val createtime: Long, val accesstime: Long,val phase : Phase.T,val round:Int)

object GameData {

  implicit def converttoJ(g : GameData) : String = {
    val j : JsValue = Json.obj(
      S.gameid -> g.gameid,
      S.civ -> g.civ,
      S.createtime -> g.createtime,
      S.accesstime -> g.accesstime,
      "phase" -> g.phase,
      "round" -> g.round
    )
    j.toString()
  }

}
