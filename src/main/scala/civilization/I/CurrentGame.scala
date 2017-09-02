package civilization.I

import java.util.Calendar

import civilization.io.fromjson._
import civilization.objects._
import play.api.libs.json.{JsValue, Json}

case class CurrentGame(val gameid: Int, val civ: Civilization.T, val createtime: Long = Calendar.getInstance().getTime.getTime, val accesstime: Long = Calendar.getInstance().getTime.getTime)

object CurrentGame {

  implicit def converttoJ(g: CurrentGame): String = {
    val j: JsValue = Json.obj(
      S.gameid -> g.gameid,
      S.civ -> g.civ,
      S.createtime -> g.createtime,
      S.accesstime -> g.accesstime
    )
    j.toString()
  }

  implicit def convertFromJ(s: String): CurrentGame = {
    val j: JsValue = toJ(s)
    val gameid = (j \ S.gameid).get.as[Int]
    val civ = (j \ S.civ).get.as[Civilization.T]
    val createtime = (j \ S.createtime).get.as[Long]
    val accesstime = (j \ S.accesstime).get.as[Long]
    CurrentGame(gameid, civ, createtime, accesstime)
  }

}
