package civilization

package object objects {

  val MINTRADE: Int = 6
  val TRADEMAX: Int = 27
  val TILESIZE: Int = 4
  val DEFAULTTRADEFORPROD = 3
  val DEFAULTPRODFORTRADE = 1
  val UNITLEVELSIZE = 4
  val UNITSBATTLE = 3
  val IRONSTRENGTH = 3
  val MAXLOOTTRADE = 3
  val WONDERWINDOW = 4
  val COINSCAPACITY = 4
  val CULTURECITY = 1
  val CULTURECAPITAL = 1

  case class GameConfig(val ironincreasedefend: Boolean)


  type TileTerrain = Array[Array[Square]]

  case class HutVillage(var hv: HutVillage.T, var resource: Resource.T) {
    def ==(that: HutVillage): Boolean =
      hv == that.hv && resource == that.resource
  }

  case class HVResource(val hv: Option[HutVillage.T], val resource: Resource.T)

  case class Tokens(val numofTrade: Int, val numofProduction: Int, val numofCulture: Int, val numofBattle: Int, val numofCoins: Int)

  case class Square(val terrain: Terrain.T, val hv: HutVillage.T, val resource: Option[Resource.T], val naturalwonder: Boolean, val token: Tokens)

  case class Building(val name: BuildingName.T, val cost: Int, val star: Option[Boolean], val tokens: Tokens, val upgrade: Option[BuildingName.T], val terrain: Option[Terrain.T]) {
    def isStar: Boolean = star.isDefined && star.get
  }


  case class TilesRead(val name: String, val tile: Tile)

  object P {
    def constructEmpty: P = P(-1, -1)
  }

  case class P(val row: Int, val col: Int) {
    //    >def +(that: P) = row == that.row && col == that.col

    def empty: Boolean = row == -1 && col == -1
  }

  case class Tile(val terrain: TileTerrain, val civ: Option[Civilization.T], val suggestedcapital: Option[P]) {
    def civhome: Boolean = civ.isDefined
  }

  case class Player(civ: Civilization.T)

  case class City(val civ: Civilization.T, val citytype: City.T) {
    def defenceStrength(): Int = {
      citytype match {
        case City.Capital => 12
        case City.Normal => 6
        case City.WalledCapital => 16
        case City.WalledNormal => 10
      }
    }

    def belongsTo(civ: Civilization.T): Boolean = this.civ == civ
  }

  object Resource extends Enumeration {
    type T = Value
    val Wheat, Silk, Incense, Iron, Coin, Spy, Uranium, Culture = Value
  }

  object Terrain extends Enumeration {
    type T = Value
    val Mountain, Forest, Grassland, Desert, Water = Value
  }

  object HutVillage extends Enumeration {
    type T = Value
    val Hut, Village = Value
  }

  object Civilization extends Enumeration {
    type T = Value
    val America, China, Egypt, Germany, Rome, Russia, Spain, Arabs = Value
  }

  object City extends Enumeration {
    type T = Value
    val Capital, Normal, WalledCapital, WalledNormal = Value

    def isCapital(c: Value): Boolean = (c == Capital || c == WalledCapital)
  }

  object Figure extends Enumeration {
    type T = Value
    val Army, Scout = Value
  }

  object Phase extends Enumeration {
    type T = Value
    val Ancient, Medieval, Modern, Any = Value
  }

  object Token extends Enumeration {
    type T = Value
    val Trade, Production, Culture = Value
  }

  object Orientation extends Enumeration {
    type T = Value
    val Left, Up, Down, Right = Value
  }

  object TurnPhase extends Enumeration {
    type T = Value
    val StartOfTurn, Trade, CityManagement, Movement, Research = Value

    def turnAction(t: Value): Boolean = t != Trade && t != Research
  }

  object CombatUnitType extends Enumeration {
    type T = Value
    val Artillery, Infantry, Mounted, Aircraft = Value

    def trumpover(c1: CombatUnitType.T, c2: CombatUnitType.T): Boolean =
      if (c1 == Aircraft) true
      else if (c1 == Infantry && c2 == Mounted) true
      else if (c1 == Mounted && c2 == Artillery) true
      else if (c1 == Artillery && c2 == Infantry) true
      else false
  }

  case class CombatUnitStrength() {
    private val s: Array[Int] = Array(0, 0, 0, 0)

    def getStrength(c: CombatUnitType.T): Int = s(c.id)

    def setStrength(c: CombatUnitType.T, newval: Int) = {
      require(newval >= 0 && newval <= 3)
      s(c.id) = newval
    }
  }

  case class CombatUnit(val utype: CombatUnitType.T, val strength: Array[Int]) {
    def ==(v: CombatUnit): Boolean = {
      if (this.utype != v.utype) return false
      for (i <- 0 until this.strength.length)
        if (this.strength(i) != v.strength(i)) return false
      true
    }

    def getStrength(s: CombatUnitStrength): Int =
      strength(s.getStrength(utype))

  }

  case class BattleStart(val attacker: Seq[CombatUnit], val defender: Seq[CombatUnit])

  object GovernmentName extends Enumeration {
    type T = Value
    val
    Communism,
    Democracy,
    Monarchy,
    Republic,
    Despotism,
    Feudalism,
    Anarchy,
    Fundamentalism = Value
  }

  object BuildingName extends Enumeration {
    type T = Value
    val
    Harbor,
    Shipyard,
    TradingPost,
    Workshop,
    IronMine,
    Library,
    University,
    Granary,
    Aqueduct,
    Market,
    Bank,
    Temple,
    Cathedral,
    Barracks,
    Academy = Value
  }

  object TechnologyName extends Enumeration {
    type T = Value
    val
    Irrigation,
    Chivalry,
    Construction,
    MetalCasting,
    Communism,
    MilitaryScience,
    SteamPower,
    GunPowder,
    NuclearFusion,
    ReplaceableParts,
    Ballistics,
    MassMedia,
    Computers,
    Flight,
    PrintingPress,
    Democracy,
    Monarchy,
    Sailing,
    Mathematics,
    CivilService,
    Engineering,
    Metallurgy,
    CodeOfLaw,
    HorsebackRiding,
    Writing,
    Pottery,
    Philosophy,
    Navigation,
    AnimalHusbandry,
    Currency,
    Masonry,
    Plastics,
    Ecology,
    Mysticism,
    RailRoad,
    Biology,
    Theology,
    Agriculture,
    Education,
    Combustion,
    Navy,
    Logistics,
    Bureaucracy,
    Banking = Value

    def isCoinTechnology(t: Value): Boolean = (t == Pottery || t == CodeOfLaw)
  }

  def levelTrade(level: Int): Int = MINTRADE + (level - 1) * 5

  def tradeToLevel(trade: Int): Int = (trade - 1) / 5

  case class CivilizationG(val civ: Civilization.T, val tech: TechnologyName.T, val gover: GovernmentName.T, val desc: String, val notimplemented: Option[Boolean])


  case class TechnologyUnit(val unit: Option[CombatUnitType.T], val level: Int)

  /** Technology dictionary
    *
    * @param tech  Technology name
    * @param gover Option,if technology enables government
    * @param level Level of this technology
    */
  case class Technology(val tech: TechnologyName.T, val gover: Option[GovernmentName.T], val level: Int, val notimplemented: Option[Boolean], val building: Option[BuildingName.T], val resource: Option[Resource.T], val desc: String, val resourceany: Option[Int], val unit: Option[TechnologyUnit], val coins: Option[Int])

  object WondersAge extends Enumeration {
    type T = Value
    val
    Ancient,
    Medieval,
    Modern = Value
  }

  object Wonders extends Enumeration {
    type T = Value
    val
    ChichenItza,
    StatueofZeus,
    Stonehenge,
    TheColossus,
    TheGreatLighthouse,
    TheGreatWall,
    TheHangingGardens,
    TheOracle,
    ThePyramids,
    AngkorWat,
    BrandenburgGate,
    HimejiSamuraiCastle,
    LeonardosWorkshop,
    MachuPichu,
    NotreDame,
    PorcelainTower,
    TajMahal,
    TheLouvre,
    BigBen,
    CristoRedentor,
    PanamaCanal,
    StatueofLiberty,
    SydneyOperaHouse,
    TheInternet,
    TheKremlin,
    ThePentagon,
    UnitedNations
    = Value
  }

  object GreatPersonTypeName extends Enumeration {
    type T = Value
    val Artist, Builder, General, Humanitarian, Merchant, Scientist
    = Value
  }

  case class GreatPersonType(val name: GreatPersonTypeName.T, val tokens: Tokens)

  object GreatPersonName extends Enumeration {
    type T = Value
    val
    AkiraKurosawa, FridaKahlo, JerryGarcia, MarkTwain, Michelangelo, Valmiki, WilliamShakespeare,
    AdaLovelace, Archimedes, FrankLloydWright, HenryFord, NikolaTesla, OrvilleWright, ThomasEdison,
    GeorgyZhukov, GustavusAdolphus, Hannibal, JoanofArc, KhalidibnalWalid, Leonidas, SunTzu,
    DrMartinLutherKing, FlorenceNightingale, JacquesCousteau, JimHenson, MotherTeresa, StFrancisofAssisi, SusanBAnthony,
    APGiannini, AdamSmith, AndrewCarnegie, CaptainJamesCook, LorenzodeMedici, MarcoPolo, ZhengHe,
    AlanTuring, AlbertEinstein, CharlesDarwin, GalileoGalilei, LouisPasteur, MarieCurie, SirIsaacNewton
    = Value
  }

  case class GreatPerson(val name: GreatPersonName.T, val notimplemented: Option[Boolean], val short: String, val ptype: GreatPersonTypeName.T, val phase: Option[TurnPhase.T], val desc: String)

  object CultureCardName extends Enumeration {
    type T = Value
    val
    AGiftfromAfar, BarbarianEncampment, BreadandCircuses, Counterfeiters, Defection, Disoriented, Drought, DutyandHonor, ExchangeofIdeas,
    Migrants, Sabotage, TheCitizensareRevolting, WeLovetheDespotDay,
    AGenerousGift, Catastrophe, Colonists, Deforestation, Depression, Flooding, JoustingTourney, KnowledgeShared, Lost,
    MassDefection, Nationalism, RoamingHorde, TheCitizensareRevoltingII, WeLovetheQueenDay,
    APrincelyGift, BankScandal, Disaster, Immigrants, OutofPosition, Patriotism, PrimetimeTV, SupplyDrop, ThinkTank, WeLovethePresidentDay,
    WholesaleDefection
    = Value
  }

  case class CultureCard(val name: CultureCardName.T, val level: Int, val notimplemented: Option[Boolean], val num: Int, val phase: Option[TurnPhase.T], val desc: String)

}
