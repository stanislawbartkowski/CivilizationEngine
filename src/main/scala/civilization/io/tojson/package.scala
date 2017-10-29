package civilization.io

import civilization.gameboard._
import civilization.objects._
import play.api.libs.functional.syntax._
import play.api.libs.json
import play.api.libs.json._


/** Helper methods to convert objects into JSON */

package object tojson extends ImplicitMiximToJson {

  implicit val hutVillageWrites: Writes[HutVillage] = (
    (JsPath \ S.hutvillage).write[HutVillage.T] and
      (JsPath \ S.resource).write[Resource.T]
    ) (unlift(HutVillage.unapply))

  implicit val pointWrites: Writes[P] = (
    (JsPath \ S.row).write[Int] and
      (JsPath \ S.col).write[Int]
    ) (unlift(P.unapply))

  implicit val cityWrites: Writes[City] = (
    (JsPath \ S.civ).write[Civilization.T] and
      (JsPath \ S.citytype).write[City.T]
    ) (unlift(City.unapply))

  implicit val resourceWrites: Writes[Resources] = new Writes[Resources] {

    def writes(m: Resources) = Json.obj(
      S.hutvillages -> m.hv,
      S.hutvillagesused -> m.hvused,
      S.resources -> m.resou
    )
  }

  implicit val playertechnologyWrites: Writes[PlayerTechnology] = new Writes[PlayerTechnology] {
    def writes(m: PlayerTechnology) = Json.obj(
      S.tech -> m.tech
    )
  }

  implicit val playerdeckWrites: Writes[PlayerDeck] = new Writes[PlayerDeck] {
    def writes(m: PlayerDeck) = Json.obj(
      S.civ -> m.civ,
      S.tech -> m.tech,
      S.units -> m.units,
      S.resources -> m.resou
    )
  }

  implicit val playerfiguresWrites: Writes[PlayerFigures] = new Writes[PlayerFigures] {
    def writes(m: PlayerFigures) = Json.obj(
      S.civ -> {
        if (m.civ == null) json.JsNull else m.civ
      },
      S.numberofArmies -> m.numberofArmies,
      S.numberofScouts -> m.numberofScouts
    )
  }

  implicit val mapsqaureWrites: Writes[MapSquare] = new Writes[MapSquare] {
    def writes(m: MapSquare) = Json.obj(
      S.hutvillage -> {
        m.hv
      },
      S.city -> {
        m.city
      }
    )
  }

  implicit val marketWrites: Writes[Market] = new Writes[Market] {
    def writes(m: Market) = Json.obj(
      S.units -> m.units,
      S.killedunits -> m.killedunits
    )
  }


  implicit val gameboardWrites: Writes[GameBoard] = new Writes[GameBoard] {
    def writes(m: GameBoard) = Json.obj(
      S.players -> m.players,
      S.map -> m.map.map,
      S.resources -> m.resources,
      S.market -> m.market
    )
  }

  implicit val gameboardFigures: Writes[Figures] = new Writes[Figures] {
    def writes(f: Figures) = Json.obj(
      S.numberofArmies -> f.numberofArmies,
      S.numberofScouts -> f.numberofScouts
    )
  }

  implicit val maptileWrites: Writes[MapTile] = new Writes[MapTile] {
    def writes(m: MapTile) = Json.obj(
      S.tilename -> m.tname,
      S.p -> m.p,
      S.orientation -> {
        if (m.orientation == null) json.JsNull else m.orientation
      },
      S.squares -> m.mapsquares
    )
  }

  implicit val metadataWrites: Writes[GameMetaData] = new Writes[GameMetaData] {
    def writes(m: GameMetaData) = Json.obj(
      S.version -> m.version,
      S.createtime -> m.createtime,
      S.accesstime -> m.accesstime,
      S.desc -> m.desc
    )
  }

  implicit val gameresourcesWrites : Writes[GameResources] = new Writes[GameResources]  {
    def writes(m: GameResources) = {
       var l: Seq[JsValue] = m.table.map(e => Json.obj(
         S.resource -> e._1,
         S.num -> e._2
       )).toSeq
      JsArray(l)
    }
  }

  implicit val commandparamWrites: Writes[CommandValues] = new Writes[CommandValues] {
    def writes(m: CommandValues) = Json.obj(
      S.command -> m.command,
      S.civ -> m.civ,
      S.p -> {
        if (m.p == null) json.JsNull else m.p
      },
      S.param -> {
        if (m.param == null) json.JsNull else m.param
      }
    )
  }

  implicit val commandparamCombatUnit: Writes[CombatUnit] = new Writes[CombatUnit] {
    def writes(m: CombatUnit ) = Json.obj(
      S.unitname -> m.utype,
      S.unitstrength -> m.strength
    )
  }

  def writeCivilizationT(c: Civilization.T): JsValue = Json.toJson(c)

  def writeHutVillage(h: HutVillage): JsValue = Json.toJson(h)

  def writeResources(m: Resources): JsValue = Json.toJson(m)

  def writePlayerTechnology(t: PlayerTechnology): JsValue = Json.toJson(t)

  def writeSeqPlayerDeck(d: Seq[PlayerDeck]): JsValue = Json.toJson(d)

  def writeSeqOfMapTile(d: Seq[MapTile]): JsValue = Json.toJson(d)

  def writesGameBoard(d: GameBoard): JsValue = Json.toJson(d)

  def writesP(p: P): JsValue = writesPoint(p)

  def writesFigures(f: Figures) = Json.toJson(f)

  def writeListOfCiv(filt: Civilization.T => Boolean): JsValue = {
    val j: Seq[Civilization.T] = Civilization.values.toList.filter(filt)
    Json.toJson(j)
  }

  def writeCommandValues(m: CommandValues): JsValue = Json.toJson(m)

  def writeMetaData(m: GameMetaData): JsValue = Json.toJson(m)

  def writeCombatUnit(m: CombatUnit): JsValue = Json.toJson(m)

}
