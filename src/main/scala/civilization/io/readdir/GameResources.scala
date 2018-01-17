package civilization.io.readdir

import civilization.I.CurrentGame
import civilization.objects._
import civilization.gameboard._
import civilization.io.readdir._
import civilization.io.tojson._
import play.api.libs.json.{JsValue, Json}

case class GameResources private(val civ: Seq[CivilizationG], val tech: Seq[Technology], val wonders: Seq[WondersOfTheWorld], val buldings: Seq[Building])

object GameResources {

  private var _instance: GameResources = null

  def instance() = {
    if (_instance == null)
      _instance = new GameResources(readListOfCivs, readTechnologies, readListOfWonders, readListOfBuildings)
    _instance
  }

  implicit def convert(g: GameResources): JsValue = {
    val j: JsValue = Json.obj(
      S.civ -> writeListOfCivs(g.civ),
      S.tech -> writeListOfTechs(g.tech),
      S.wonders -> writeListOfWonders(g.wonders),
      S.buildings -> writeListOfBuilding(g.buldings)
    )
    j
  }
}
