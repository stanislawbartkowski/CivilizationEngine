package civilization.io

import java.io.File

import play.api.libs.json.Reads._
import play.api.libs.json._

import civilization.io.fromjson._
import civilization.objects._
import civilization.gameboard._
import civilization.gameboard.CultureTrack._
import play.api.libs.functional.syntax._

import scala.io.Source

/** Helper for reading resource data as JSON */
package object readdir extends ImplicitMiximFromJson {

  def readTestJSON(resourcefile: String): JsValue =
    Json.parse(getClass().getClassLoader.getResource(resourcefile).openStream())


  def readJournalDict : JsValue = readJSON("messages", "journal.json")

  def readJSON(resourcedir: String, resourcefile: String): JsValue = {
    val i: Iterator[String] =
      Source.fromResource(resourcedir + "/" + resourcefile).getLines()
    val s: String = i.toStream.foldLeft("")((a, b) => a + b)
    Json.parse(s)
  }

  // TODO : not always work as expected, failing for Heroku
  //  private def getDirectory(resourcedir : String) : Seq[String] = Source.fromResource(resourcedir).getLines().toSeq

  private def getDirectory(resourcedir: String): Seq[String] = {
    val fullDirName: String = getClass.getClassLoader.getResource(resourcedir).getPath
    val d = new File(fullDirName)
    d.listFiles().map(_.getName)
  }

  def readdirJSON(resourcedir: String): List[(String, JsValue)] = {
    val o: Seq[String] = getDirectory(resourcedir)
    o.map(filename => (filename, readJSON(resourcedir, filename))).toList
  }

  private def readTileFromFile(f: String): Tile = {
    val j: JsValue = readJSON("map/tiles", f)
    toTile(j)
  }

  def readListOfTiles: Seq[TilesRead] =
    // only files starting with R
    readdirJSON("map/tiles").filter(_._1.startsWith("R")).map(f => TilesRead(f._1, toTile(f._2))).toList

  def readListOfCivs: Seq[CivilizationG] = readJSON("objects", "CIVILIZATIONS.json")

  def readListOfWonders : Seq[WondersOfTheWorld] = readJSON("objects","WONDERS.json")

  def readListOfGreatPersonType : Seq[GreatPersonType] = readJSON("objects","GREATPERSON.json")

  def readListOfGreatPersons : Seq[GreatPerson] = readJSON("objects","GREATPERSONSLIST.json")

  def readListOfBuildings : Seq[Building] = readJSON("objects","BUILDINGS.json")

  def readCultureTrack : CultureTrack = readJSON("objects","CULTURETRACK.json")

  def readCultureCards : Seq[CultureCard] = readJSON("objects","CULTURECARDS.json")

  def readGameBoard(j: JsValue): GameBoard = {
    val g: GameBoard = toGameBoard(j)
    g.map.map.foreach(p => {
      p.tile = readTileFromFile(p.tname)
    })
    g
  }

  def readPlay(j: JsValue): Seq[CommandValues] = {
    val js: JsValue = (j \ "game").get
    js
  }

  def readTechnologies: Seq[Technology] = readJSON("objects", "TECHNOLOGIES.json")

  case class NumCombatUnit(val no: Int, val unit: CombatUnit)

  implicit val nocombatunitReads: Reads[NumCombatUnit] = (
    (JsPath \ S.num).read[Int] and (JsPath \ "unit").read[CombatUnit]
    ) (NumCombatUnit.apply _)


  def readListOfUnits: Seq[CombatUnit] = {
    val j: JsValue = readJSON("map/market", "UNITS.json")
    val list: Seq[NumCombatUnit] = j.as[Seq[NumCombatUnit]]
    // 2017/12/24
    // important until (not to)
    list.map(se => for (i <- 0 until se.no) yield se.unit) flatten
  }

}