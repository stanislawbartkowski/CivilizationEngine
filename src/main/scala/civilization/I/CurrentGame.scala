package civilization.I

import civilization.objects._
import play.api.libs.json
import play.api.libs.json.{JsValue, Json}
import civilization.io.fromjson._

case class CurrentGame(val gameid : Int, val civ : Civilization.T, val accesstime : Long)

object CurrentGame {

  implicit def converttoJ(g : CurrentGame) : String = {
    val j : JsValue = Json.obj(
      "gameid" -> g.gameid,
      S.civ -> g.civ,
      "accesstime" -> g.accesstime
    )
    j.toString()
  }

  implicit def convertFromJ(s : String) : CurrentGame = {
    val j : JsValue = toJ(s)
    val gameid = (j \ "gameid").get.as[Int]
    val civ = (j \ S.civ).get.as[Civilization.T]
    val accesstime = (j \ "accesstime").get.as[Long]
    CurrentGame(gameid,civ,accesstime)
  }

}
