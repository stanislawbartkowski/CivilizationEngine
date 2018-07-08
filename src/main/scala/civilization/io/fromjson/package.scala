package civilization.io

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import civilization.objects._
import civilization.gameboard._
import civilization.gameboard.CultureTrack._
import civilization.io.readdir.Param._

package object fromjson extends ImplicitMiximFromJson {

  implicit val pointReads: Reads[P] = (
    (JsPath \ S.row).read[Int] and (JsPath \ S.col).read[Int]
    ) (P.apply _)


  object EnumUtils {
    def enumReads[E <: Enumeration](enum: E): Reads[E#Value] = new Reads[E#Value] {
      def reads(json: JsValue): JsResult[E#Value] = json match {
        case JsString(s) => {
          try {
            JsSuccess(enum.withName(s))
          } catch {
            case _: NoSuchElementException => JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
          }
        }
        case JsNull => {
          JsSuccess(null)
        }
        case _ => JsError("String value expected")
      }
    }
  }

  implicit val civilizationReads: Reads[Civilization.Value] = EnumUtils.enumReads(Civilization)
  implicit val resourceReads: Reads[Resource.Value] = EnumUtils.enumReads(Resource)
  implicit val terrainReads: Reads[Terrain.Value] = EnumUtils.enumReads(Terrain)
  implicit val hutvillageReads: Reads[HutVillage.Value] = EnumUtils.enumReads(HutVillage)
  implicit val cityTypeReads: Reads[City.Value] = EnumUtils.enumReads(City)
  implicit val orientationReads: Reads[Orientation.Value] = EnumUtils.enumReads(Orientation)
  implicit val commandReads: Reads[Command.Value] = EnumUtils.enumReads(Command)
  implicit val figuresReads: Reads[Figure.Value] = EnumUtils.enumReads(Figure)
  implicit val phaseReads: Reads[TurnPhase.Value] = EnumUtils.enumReads(TurnPhase)
  implicit val technologyNameReads: Reads[TechnologyName.Value] = EnumUtils.enumReads(TechnologyName)
  implicit val governmentNameReads: Reads[GovernmentName.Value] = EnumUtils.enumReads(GovernmentName)
  implicit val inittypeNameReads: Reads[CombatUnitType.Value] = EnumUtils.enumReads(CombatUnitType)
  implicit val inittypeWondersAge: Reads[WondersAge.Value] = EnumUtils.enumReads(WondersAge)
  implicit val inittypeWonders: Reads[Wonders.Value] = EnumUtils.enumReads(Wonders)
  implicit val enumtypeBuildingName: Reads[BuildingName.Value] = EnumUtils.enumReads(BuildingName)
  implicit val enumtypeGreatPersonTypeName: Reads[GreatPersonTypeName.Value] = EnumUtils.enumReads(GreatPersonTypeName)
  implicit val enumtypeGreatPersonName: Reads[GreatPersonName.Value] = EnumUtils.enumReads(GreatPersonName)
  implicit val enumtypeCultureCardName: Reads[CultureCardName.Value] = EnumUtils.enumReads(CultureCardName)

  implicit val towinnerlootReads: Reads[WinnerLoot] = new Reads[WinnerLoot] {
    def reads(json: JsValue): JsResult[WinnerLoot] = {
      var hv: Option[HutVillage.T] = None
      var res: Option[Resource.T] = None
      var trade: Boolean = false
      var culture: Boolean = false
      val s = json.as[String]
      if (s == S.trade) trade = true
      else {
        hv = json.asOpt[HutVillage.T]
        if (hv.isEmpty) res = json.asOpt[Resource.T]
        if (res.isEmpty && hv.isEmpty)
          return JsError(json.toString() + " improper loot name")

      }
      JsSuccess(WinnerLoot(hv, res, trade, culture))
    }
  }

  implicit val tokensReads: Reads[Tokens] = new Reads[Tokens] {
    def reads(json: JsValue): JsResult[Tokens] = {
      val numofProduction: Int = (json \ S.productionT).asOpt[Int].getOrElse(0)
      val numofCulture: Int = (json \ S.cultureT).asOpt[Int].getOrElse(0)
      val numofTrade: Int = (json \ S.tradeT).asOpt[Int].getOrElse(0)
      val numofBattle: Int = (json \ S.battleT).asOpt[Int].getOrElse(0)
      val numofCoins: Int = (json \ S.coinT).asOpt[Int].getOrElse(0)
      JsSuccess(Tokens(numofTrade, numofProduction, numofCulture, numofBattle, numofCoins))
    }
  }

  implicit val buildingpointReads: Reads[BuildingPoint] = (
    (JsPath \ S.p).read[P] and
      (JsPath \ S.building).readNullable[BuildingName.T] and
      (JsPath \ S.wonder).readNullable[Wonders.T] and
      (JsPath \ S.greatperson).readNullable[GreatPersonName.T]
    ) (BuildingPoint.apply _)

  implicit val builingReads: Reads[Building] = (
    (JsPath \ S.name).read[BuildingName.T] and
      (JsPath \ S.cost).read[Int] and
      (JsPath \ S.star).readNullable[Boolean] and
      (JsPath \ S.tokens).read[Tokens] and
      (JsPath \ S.upgrade).readNullable[BuildingName.T] and
      (JsPath \ S.terrain).readNullable[Terrain.T]
    ) (Building.apply _)

  implicit val culturetrackconstReads: Reads[CultureTrackCost] = (
    (JsPath \ S.culture).read[Int] and
      (JsPath \ S.trade).read[Int]
    ) (CultureTrackCost.apply _)

  implicit val culturetracksegmentReads: Reads[CultureTrackSegment] = (
    (JsPath \ S.last).read[Int] and
      (JsPath \ S.cost).read[CultureTrackCost] and
      (JsPath \ S.greatperson).read[Seq[Int]]
    ) (CultureTrackSegment.apply _)

  implicit val squareReads: Reads[Square] = new Reads[Square] {
    def reads(json: JsValue): JsResult[Square] = {
      val terrain: Terrain.T = (json \ S.terrain).as[Terrain.T]
      val hv: HutVillage.T = (json \ S.hutvillage).asOpt[HutVillage.T].getOrElse(null)
      val resource: Option[Resource.T] = (json \ S.resource).asOpt[Resource.T]
      val naturalwonder: Boolean = (json \ "naturalwonder").asOpt[Boolean].getOrElse(false)
      val token: Tokens = (json \ S.tokens).asOpt[Tokens].getOrElse(Tokens(0, 0, 0, 0, 0))
      JsSuccess(Square(terrain, hv, resource, naturalwonder, token))
    }
  }

  implicit val greatpersonTypeReads : Reads[GreatPersonType] = (
    (JsPath \ S.name).read[GreatPersonTypeName.T] and
      (JsPath \ S.tokens).read[Tokens]
    ) (GreatPersonType.apply _)

  implicit val greatpersonReads : Reads[GreatPerson] = (
    (JsPath \ S.name).read[GreatPersonName.T] and
      (JsPath \ S.notimplemented).readNullable[Boolean] and
        (JsPath \ S.nameshort).read[String] and
        (JsPath \ S.persontype).read[GreatPersonTypeName.T] and
        (JsPath \ S.phase).readNullable[TurnPhase.T] and
        (JsPath \ S.desc).read[String]
  ) (GreatPerson.apply _)

  implicit val culturecardReads : Reads[CultureCard] = (
    (JsPath \ S.name).read[CultureCardName.T] and
      (JsPath \S.level).read[Int] and
      (JsPath \ S.notimplemented).readNullable[Boolean] and
      (JsPath \ S.num).read[Int] and
      (JsPath \S.phase).readNullable[TurnPhase.T] and
      (JsPath \ S.desc).read[String]
  ) (CultureCard.apply _)

  implicit val playefiguresReads: Reads[PlayerFigures] = new Reads[PlayerFigures] {
    def reads(json: JsValue): JsResult[PlayerFigures] = {
      val civ: Civilization.T = (json \ S.civ).as[Civilization.T]
      val numberofarmies: Int = (json \ S.numberofArmies).asOpt[Int].getOrElse(0)
      val numberofscouts: Int = (json \ S.numberofScouts).asOpt[Int].getOrElse(0)
      if (numberofarmies == 0 && numberofscouts == 0) return JsError(json.toString() + " (playerfigures) either numberofArmies or numberofScouts should be greater then 0")
      JsSuccess(PlayerFigures(civ, numberofarmies, numberofscouts))
    }
  }

  implicit val figurestomoveReads: Reads[Figures] = new Reads[Figures] {
    def reads(json: JsValue): JsResult[Figures] = {
      val numberofarmies: Int = (json \ S.numberofArmies).asOpt[Int].getOrElse(0)
      val numberofscouts: Int = (json \ S.numberofScouts).asOpt[Int].getOrElse(0)
      if (numberofarmies == 0 && numberofscouts == 0) return JsError(json.toString() + " (figurestomove) either numberofArmies or numberofScouts should be greater then 0")
      JsSuccess(Figures(numberofarmies, numberofscouts))
    }
  }

  implicit val commandparamReads: Reads[CommandValues] = new Reads[CommandValues] {
    def reads(json: JsValue): JsResult[CommandValues] = {
      val command: Command.T = (json \ S.command).as[Command.T]
      val civ: Civilization.T = (json \ S.civ).as[Civilization.T]
      val p: P = (json \ S.p).asOpt[P].getOrElse(null)
      val param: JsValue = (json \ S.param).asOpt[JsValue].getOrElse(null)
      JsSuccess(CommandValues(command, civ, p, param))
    }
  }


  implicit val cityReads: Reads[City] = (
    (JsPath \ S.civ).read[Civilization.T] and (JsPath \ S.citytype).read[City.T]
    ) (City.apply _)

  implicit val techbuildReads: Reads[TechnologyUnit] = (
    (JsPath \ S.name).readNullable[CombatUnitType.T] and
      (JsPath \ S.level).read[Int]
    ) (TechnologyUnit.apply _)

  implicit val techReads: Reads[Technology] = (
    (JsPath \ S.name).read[TechnologyName.T] and
      (JsPath \ S.gover).readNullable[GovernmentName.T] and
      (JsPath \ S.level).read[Int] and
      (JsPath \ S.notimplemented).readNullable[Boolean] and
      (JsPath \ S.building).readNullable[BuildingName.T] and
      (JsPath \ S.resource).readNullable[Resource.T] and
      (JsPath \ S.desc).read[String] and
      (JsPath \ S.resourceany).readNullable[Int] and
      (JsPath \ S.units).readNullable[TechnologyUnit] and
      (JsPath \ S.coin).readNullable[Int]
    ) (Technology.apply _)

  def listReads[T](length: Int)(implicit anyListReads: Reads[Array[T]]): Reads[Array[T]] = {
    js: JsValue =>
      anyListReads.reads(js).filter(JsError(JsonValidationError(s"Length of the list must be $length")))(_.size == length)
  }


  implicit val combatunitReads: Reads[CombatUnit] = (
    (JsPath \ S.unitname).read[CombatUnitType.T] and (JsPath \ S.unitstrength).read[Array[Int]](listReads[Int](UNITLEVELSIZE))
    ) (CombatUnit.apply _)

  implicit val tileReads: Reads[Tile] = new Reads[Tile] {
    def reads(json: JsValue): JsResult[Tile] = {
      val terrain: Array[Array[Square]] = (json \ S.squares).as[Array[Array[Square]]]
      // validate size
      if (terrain.length != TILESIZE) return JsError("squares size should be " + TILESIZE + " x " + TILESIZE + " matrix. Found " + terrain.length)
      for (i <- 0 until TILESIZE)
        if (terrain(i).length != TILESIZE)
          return JsError("All rows should have " + TILESIZE + " squares. For row + " + i + " found " + terrain(i))

      val homeciv: Option[Civilization.T] = (json \ S.civ).asOpt[Civilization.T]
      val suggestedcapital: Option[P] = (json \ "suggestedcapital").asOpt[P]
      JsSuccess(Tile(terrain, homeciv, suggestedcapital))
    }
  }

  implicit val pattermapReads: Reads[PatternMap] = new Reads[PatternMap] {
    def reads(json: JsValue): JsResult[PatternMap] = {
      val p: P = (json \ S.p).as[P]
      val orientation: Option[Orientation.T] = (json \ S.orientation).asOpt[Orientation.T]
      JsSuccess(PatternMap(p, orientation))
    }
  }

  implicit val mapsquareReads: Reads[MapSquare] = new Reads[MapSquare] {
    def reads(json: JsValue): JsResult[MapSquare] = {
      val hv: Option[HutVillage] = (json \ S.hutvillage).asOpt[Option[HutVillage]].getOrElse(None)
      val figures: PlayerFigures = (json \ "figures").asOpt[PlayerFigures].getOrElse(null)
      val city: Option[City] = (json \ S.city).asOpt[Option[City]].getOrElse(None)
      val building: BuildingName.T = (json \ S.building).asOpt[BuildingName.T].getOrElse(null)
      val ma: MapSquare = MapSquare(hv, city)
      if (figures != null) {
        ma.figures.civ = figures.civ
        ma.figures.numberofScouts = figures.numberofScouts
        ma.figures.numberofArmies = figures.numberofArmies
      }
      if (building != null) ma.setBuilding(building)
      JsSuccess(ma)
    }
  }

  implicit val maptileReads: Reads[MapTile] = new Reads[MapTile] {
    def reads(json: JsValue): JsResult[MapTile] = {
      val tname: String = (json \ S.tilename).as[String]
      val p: P = (json \ S.p).as[P]
      var orientation: Option[Orientation.T] = (json \ S.orientation).asOpt[Orientation.T]
      // 2017/10/29 : very strange but null transformed to Some(null)
      if (orientation == Some(null)) orientation = None
      var squares: Array[Array[MapSquare]] = (json \ S.squares).asOpt[Array[Array[MapSquare]]].getOrElse(null)
      if (squares == null) squares = genEmptySquares
      if (squares.length != TILESIZE) return JsError("squares size should be " + TILESIZE + " x " + TILESIZE + " matrix. Found " + squares.length)
      for (i <- 0 until TILESIZE)
        if (squares(i).length != TILESIZE)
          return JsError("All rows should have " + TILESIZE + " squares. For row + " + i + " found " + squares(i))
      JsSuccess(MapTile(tname, p, orientation, squares))
    }
  }

  implicit val readsPlayerTechnology: Reads[PlayerTechnology] = (
    (JsPath \ S.tech).read[TechnologyName.T] and
      (JsPath \ S.initial).readNullable[Boolean]
//      (JsPath \ S.coins).readNullable[Int]
    ) (PlayerTechnology.apply _)

  implicit val readCivilization: Reads[CivilizationG] = (
    (JsPath \ S.civ).read[Civilization.T] and
      (JsPath \ S.tech).read[TechnologyName.T] and
      (JsPath \ S.gover).read[GovernmentName.T] and
      (JsPath \ S.desc).read[String] and
      (JsPath \ S.notimplemented).readNullable[Boolean]
    ) (CivilizationG.apply _)

  implicit val playerdeckReads: Reads[PlayerDeck] = new Reads[PlayerDeck] {
    def reads(json: JsValue): JsResult[PlayerDeck] = {
      val civ: Civilization.T = (json \ S.civ).as[Civilization.T]
      val gover: GovernmentName.T = (json \ S.gover).as[GovernmentName.T]
      val tech: Seq[PlayerTechnology] = (json \ S.tech).as[Seq[PlayerTechnology]]
      val units: Seq[CombatUnit] = (json \ S.units).as[Seq[CombatUnit]]
      val resou: BoardResources = (json \ S.resources).as[BoardResources]
      val cultureprogress: Option[Int] = (json \ S.cultureprogress).asOpt[Int]
      JsSuccess(PlayerDeck(civ, tech, units, resou, gover, if (cultureprogress.isEmpty) 0 else cultureprogress.get))
    }
  }

  implicit val hutvillageMyReads: Reads[HutVillage] = (
    (JsPath \ S.hutvillage).read[HutVillage.T] and (JsPath \ S.resource).read[Resource.T]
    ) (HutVillage.apply _)

  implicit val hutvillageresourceMyReads: Reads[HVResource] = (
    (JsPath \ S.hutvillage).readNullable[HutVillage.T] and (JsPath \ S.resource).read[Resource.T]
    ) (HVResource.apply _)

  implicit val readRC: Reads[HVResourcesForCiv] = (
    (JsPath \ S.resource).read[Seq[HVResource]] and
      (JsPath \ S.civ).readNullable[Civilization.T]
    ) (HVResourcesForCiv.apply _)


  implicit val readResourceC: Reads[HVResourceCiv] = (
    (JsPath \ S.resource).read[HVResource] and
      (JsPath \ S.civ).readNullable[Civilization.T]
    ) (HVResourceCiv.apply _)

  implicit val hutvillagekOptionReads: Reads[Option[HutVillage]] = (
    JsPath.readNullable[HutVillage]
    )

  implicit val cityOptionReads: Reads[Option[City]] = (
    JsPath.readNullable[City]
    )

  implicit val readBattleStart: Reads[BattleStart] = (
    (JsPath \ S.attacker).read[Seq[CombatUnit]] and (JsPath \ S.defender).read[Seq[CombatUnit]]
    ) (BattleStart.apply _)

  implicit val marketReads: Reads[Resources] = (
    (JsPath \ S.hutvillages).read[Seq[HutVillage]] and (JsPath \ S.hutvillagesused).read[Seq[HutVillage]] and
      (JsPath \ S.resources).read[BoardResources]
    ) (Resources.apply _)

  implicit val markettReads: Reads[Market] = (
    (JsPath \ S.units).read[Seq[CombatUnit]] and
      (JsPath \ S.killedunits).read[Seq[CombatUnit]] and
      (JsPath \ S.buildings).read[BuildingsResources] and
      (JsPath \ S.wonders).read[Seq[Wonders.T]]
    ) (Market.apply _)

  implicit val gameboardReads: Reads[GameBoard] = new Reads[GameBoard] {
    def reads(json: JsValue): JsResult[GameBoard] = {
      val players: List[PlayerDeck] = (json \ S.players).as[List[PlayerDeck]]
      val map: Seq[MapTile] = (json \ S.map).as[Seq[MapTile]]
      val resources: Resources = (json \ S.resources).as[Resources]
      val market: Market = (json \ S.market).as[Market]
      val g: GameBoard = GameBoard(players, BoardMap(map), resources, market)
      // cheating
      val norotate: Option[Boolean] = (json \ "norotate").asOpt[Boolean]
      if (norotate.isDefined && norotate.get) g.norotate = true
      // cheating
      val tradecurrent: Option[Boolean] = (json \ "tradecurrent").asOpt[Boolean]
      if (tradecurrent.isDefined && tradecurrent.get) g.tradecurrent = true

      JsSuccess(g)
    }
  }

  implicit val metadataReads: Reads[GameMetaData] = (
    (JsPath \ S.version).read[Int] and
      (JsPath \ S.createtime).read[Long] and
      (JsPath \ S.accesstime).read[Long] and
      (JsPath \ S.boardmili).read[Long] and
      (JsPath \ S.desc).read[String]
    ) (GameMetaData.apply _)

  implicit val takeWinnerLootReads: Reads[TakeWinnerLoot] = (
    (JsPath \ S.winner).read[Civilization.T] and
      (JsPath \ S.loser).read[Civilization.T] and
      (JsPath \ S.winnerloot).read[WinnerLoot] and
      (JsPath \ S.resource).readNullable[Resource.T] and
      (JsPath \ S.trade).read[Int]
    ) (TakeWinnerLoot.apply _)

  implicit val WondersDiscountReads: Reads[WondersDiscount] = (
    (JsPath \ S.cost).read[Int] and
      (JsPath \ S.tech).read[TechnologyName.T]
    ) (WondersDiscount.apply _)

  implicit val WondersReads: Reads[WondersOfTheWorld] = (
    (JsPath \ S.name).read[Wonders.T] and
      (JsPath \ S.phase).readNullable[TurnPhase.T] and
      (JsPath \ S.age).read[WondersAge.T] and
      (JsPath \ S.cost).read[Int] and
      (JsPath \ S.discount).readNullable[WondersDiscount] and
      (JsPath \ S.desc).read[String] and
      (JsPath \ S.nameshort).read[String] and
      (JsPath \ S.notimplemented).readNullable[Boolean]
    ) (WondersOfTheWorld.apply _)

  trait FromJson {
    type Value
    val j: JsValue

    def to: JsResult[Value]
  }

  case class TokensJ(val j: JsValue) extends FromJson {
    type Value = Tokens

    def to: JsResult[Tokens] = (JsPath).read[Tokens].reads(j)
  }

  case class PJ(val j: JsValue) extends FromJson {
    type Value = P

    def to: JsResult[P] = (JsPath).read[P].reads(j)
  }

  case class SquareJ(val j: JsValue) extends FromJson {
    type Value = Square

    def to: JsResult[Square] = (JsPath).read[Square].reads(j)
  }

  case class TileJ(val j: JsValue) extends FromJson {
    type Value = Tile

    def to: JsResult[Tile] = (JsPath).read[Tile].reads(j)
  }

  case class PlayerDeckJ(val j: JsValue) extends FromJson {
    type Value = PlayerDeck

    def to: JsResult[PlayerDeck] = (JsPath).read[PlayerDeck].reads(j)
  }

  case class MapTileJ(val j: JsValue) extends FromJson {
    type Value = MapTile

    def to: JsResult[MapTile] = (JsPath).read[MapTile].reads(j)
  }

  case class GameBoardJ(val j: JsValue) extends FromJson {
    type Value = GameBoard

    def to: JsResult[GameBoard] = (JsPath).read[GameBoard].reads(j)
  }

  case class HutVillageJ(val j: JsValue) extends FromJson {
    type Value = HutVillage

    def to: JsResult[HutVillage] = (JsPath).read[HutVillage].reads(j)
  }

  case class TurnPhaseJ(val j: JsValue) extends FromJson {
    type Value = TurnPhase.T

    def to: JsResult[TurnPhase.T] = (JsPath).read[TurnPhase.T].reads(j)
  }

  case class OrientationJ(val j: JsValue) extends FromJson {
    type Value = Orientation.T

    def to: JsResult[Orientation.T] = (JsPath).read[Orientation.T].reads(j)
  }

  case class CommandValuesJ(val j: JsValue) extends FromJson {
    type Value = CommandValues

    def to: JsResult[CommandValues] = (JsPath).read[CommandValues].reads(j)
  }

  case class CombatUnitJ(val j: JsValue) extends FromJson {
    type Value = CombatUnit

    def to: JsResult[CombatUnit] = (JsPath).read[CombatUnit].reads(j)
  }


  case class SeqCommandValuesJ(val j: JsValue) extends FromJson {
    type Value = Seq[CommandValues]

    def to: JsResult[Seq[CommandValues]] = (JsPath).read[Seq[CommandValues]].reads(j)
  }


  case class PatterMapSeqJ(val j: JsValue) extends FromJson {
    type Value = Seq[PatternMap]

    def to: JsResult[Seq[PatternMap]] = (JsPath).read[Seq[PatternMap]].reads(j)
  }

  case class SeqTechnologyJ(val j: JsValue) extends FromJson {
    type Value = Seq[Technology]

    def to: JsResult[Seq[Technology]] = (JsPath).read[Seq[Technology]].reads(j)
  }

  case class FiguresToMoveJ(val j: JsValue) extends FromJson {
    type Value = Figures

    def to: JsResult[Figures] = (JsPath).read[Figures].reads(j)
  }

  def convert[T <: FromJson](t: T): T#Value = {
    val readResult: JsResult[T#Value] = t.to
    readResult match {
      case s: JsSuccess[T] => readResult.get
      case e: JsError => {
        println(t)
        println("Errors while converting enum " + t.getClass + " : " + JsError.toJson(e).toString())
        throw new RuntimeException
      }
    }
  }

  def eqJsParam(j1: JsValue, j2: CommandParams): Boolean = {
    val p: Option[P] = (j1 \ S.p).asOpt[P]
    if (p.isEmpty && j2.p.isDefined) return false
    if (p.isDefined && j2.p.isEmpty) return false
    if (p.isDefined && j2.p.isDefined)
      if (!(p == j2.p)) return false
    val jpar: Option[JsObject] = (j1 \ S.param).asOpt[JsObject]
    if (jpar.isEmpty && j2.param.isEmpty) return true
    if (!jpar.isDefined || !j2.param.isDefined) return false
    //    if (jpar.isDefined)
    //      if (j2.param.isEmpty) return false
    //    if (j2.param.isEmpty) return false

    //    assert(!jpar.isDefined && !j2.param.isDefined,"Not implemented for JSON")
    //    true
    // TODO: json comparison, review
    return jpar.get == j2.param.get
  }


  def toP(j: JsValue): P = toPoint(j)

  def toGameBoard(j: JsValue): GameBoard = convert[GameBoardJ](GameBoardJ(j))

  def toTile(j: JsValue): Tile = convert[TileJ](TileJ(j))

  def toHutVillage(j: JsValue): HutVillage = convert[HutVillageJ](HutVillageJ(j))

  def toJ(json: String): JsValue = Json.parse(json)

  def toParams(j: JsValue): CommandValues = convert[CommandValuesJ](CommandValuesJ(j))

  def toSeqParams(j: JsValue): Seq[CommandValues] = convert[SeqCommandValuesJ](SeqCommandValuesJ(j))

  def toTurnPhase(j: JsValue): TurnPhase.T = convert[TurnPhaseJ](TurnPhaseJ(j))

  //  def toOrientation(j: JsValue): Orientation.T = convert[OrientationJ](OrientationJ(j))

  def toFigure(j: JsValue): Figure.T = {
    j.as[Figure.T]
  }

  def toFigures(j: JsValue): Figures = convert[FiguresToMoveJ](FiguresToMoveJ(j))

  def toFiguresNull(j: JsValue): Figures = {
    if (j == null) return null
    if (j == JsNull) return null
    toFigures(j)
  }


  implicit def toTechnologies(j: JsValue): Seq[Technology] = convert[SeqTechnologyJ](SeqTechnologyJ(j))

  implicit def toCivilizations(j: JsValue): Seq[CivilizationG] = j.as[Seq[CivilizationG]]

  def toSeqPatterMap(j: JsValue): Seq[PatternMap] = convert[PatterMapSeqJ](PatterMapSeqJ(j))

  def toArrayHutVillages(j: JsValue): Array[HutVillage] = j.as[Array[HutVillage]]

  def toMetaData(j: JsValue): GameMetaData = j.as[GameMetaData]

  implicit def toSeqOfWonders(j: JsValue): Seq[WondersOfTheWorld] = j.as[Seq[WondersOfTheWorld]]

  implicit def toListOfBuildings(j: JsValue): Seq[Building] = j.as[Seq[Building]]

  implicit def toCultureTrack(j: JsValue): Array[CultureTrackSegment] = j.as[Array[CultureTrackSegment]]

  implicit def toListOfGreatPersonType(j : JsValue) : Seq[GreatPersonType] = j.as[Seq[GreatPersonType]]

  implicit def toListOfGreatPersons(j : JsValue) : Seq[GreatPerson] = j.as[Seq[GreatPerson]]

  implicit def toListOfCards(j : JsValue) : Seq[CultureCard] = j.as[Seq[CultureCard]]
}
