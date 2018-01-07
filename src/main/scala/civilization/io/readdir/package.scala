package civilization.io

import java.io.File

import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.functional.syntax._

import civilization.io.fromjson._
import civilization.objects._
import civilization.gameboard._
import civilization.action._
import civilization.helper._
import civilization.message._

import scala.collection.mutable.Buffer
import scala.io.Source

/** Helper for reading resource data as JSON */
package object readdir {

  def readTestJSON(resourcefile: String): JsValue =
    Json.parse(getClass().getClassLoader.getResource(resourcefile).openStream())


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
    return toTile(j)
  }

  def readListOfTiles: Seq[TilesRead] = {
    // only files starting with R
    readdirJSON("map/tiles").filter(_._1.startsWith("R")).map(f => TilesRead(f._1, toTile(f._2))).toList
  }

  def readListOfCivs: Seq[CivilizationG] = {
    val j: JsValue = readJSON("objects", "CIVILIZATIONS.json")
    toCivilizations(j)
  }

  def readListOfWonders : Seq[WondersOfTheWorld] = {
    val j : JsValue = readJSON("objects","WONDERS.json")
    toSeqOfWonders(j)
  }

  def readListOfBuildings : Seq[Building] = {
    val j : JsValue = readJSON("objects","BUILDINGS.json")
    toListOfBuildings(j)
  }

  def readGameBoard(j: JsValue): GameBoard = {
    //    val l: Seq[TilesRead] = readListOfTiles
    val g: GameBoard = toGameBoard(j)
    g.map.map.foreach(p => {
      //      p.tile = l.find(t => p.tname == t.name).get.tile
      p.tile = readTileFromFile(p.tname)
    })
    g
  }

  def readPlay(j: JsValue): Seq[CommandValues] = {
    val js: JsValue = (j \ "game").get
    toSeqParams(js)
  }

  def readTechnologies: Seq[Technology] = {

    val j: JsValue = readJSON("objects", "TECHNOLOGIES.json")
    toTechnologies(j)
  }

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