package civilization.io.readdir

import civilization.I.CurrentGame
import civilization.objects._
import civilization.gameboard._
import civilization.gameboard.CultureTrack._
import civilization.io.readdir._
import civilization.io.tojson._
import play.api.libs.json.{JsValue, Json}

case class GameResources private(val civ: Seq[CivilizationG], val tech: Seq[Technology], val wonders: Seq[WondersOfTheWorld], val buldings: Seq[Building], val culturetrack: CultureTrack, val greatpersontype: Seq[GreatPersonType], val greatpersons: Seq[GreatPerson], val culturecards: Seq[CultureCard])

object GameResources {

  private var _instance: GameResources = null

  def instance() = {
    if (_instance == null)
      _instance = new GameResources(readListOfCivs, readTechnologies, readListOfWonders, readListOfBuildings, readCultureTrack, readListOfGreatPersonType, readListOfGreatPersons, readCultureCards)
    _instance
  }

  implicit def convert(g: GameResources): JsValue = {
    val j: JsValue = Json.obj(
      S.civ -> writeListOfCivs(g.civ),
      S.tech -> writeListOfTechs(g.tech),
      S.wonders -> writeListOfWonders(g.wonders),
      S.buildings -> g.buldings,
      S.culturetrack -> g.culturetrack,
      "greatpersontype" -> g.greatpersontype,
      S.greatperson -> g.greatpersons,
      "cards" -> g.culturecards,
      "journal" -> readJournalDict
    )
    j
  }

  def getBuilding(b: BuildingName.T): Building = instance().buldings.find(_.name == b).get

  def getTechnology(t: TechnologyName.T): Technology = instance().tech.find(_.tech == t).get

  def getWonder(t: Wonders.T): WondersOfTheWorld = instance().wonders.find(_.name == t).get

  def getGreatPerson(p: GreatPersonName.T) = instance().greatpersons.find(_.name == p).get

  def getGreatPersonType(t: GreatPersonTypeName.T) = instance().greatpersontype.find(_.name == t).get

  def getCivilizationG(civ: Civilization.T): CivilizationG = instance().civ.find(_.civ == civ).get

  def getCultureCard(t: CultureCardName.T): CultureCard = instance().culturecards.find(_.name == t).get
}
