package civilization.io

import java.io.File

import civilization.io.fromjson._
import play.api.libs.json._
import civilization.objects.{Civilization, Command, CommandValues, HutVillage, TilesRead, TurnPhase}
import civilization.gameboard._
import civilization.action._
import civilization.helper._
import civilization.message._

import scala.collection.mutable.Buffer
import scala.io.Source

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

  def readListOfTiles: Seq[TilesRead] = {
    readdirJSON("map/tiles").map(f => TilesRead(f._1, toTile(f._2))).toList
  }

  def readGameBoard(j: JsValue): GameBoard = {
    val l: Seq[TilesRead] = readListOfTiles
    val g: GameBoard = toGameBoard(j)
    g.tech = readTechnologies
    g.map.map.foreach(p => {
      p.tile = l.find(t => p.tname == t.name).get.tile
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

}