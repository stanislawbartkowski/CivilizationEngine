package civilization

package object objects {


  type TileTerrain = Array[Array[Square]]
  val TILESIZE: Int = 4

  case class HutVillage(var hv: HutVillage.T, var resource: Resource.T)

  case class Tokens(val numofTrade: Int, val numofProduction: Int, val numofCulture: Int)

  case class Square(val terrain: Terrain.T, val hv: HutVillage.T, val resource: Resource.T, val naturalwonder: Boolean, val token: Tokens)

  case class TilesRead(val name: String, val tile: Tile)

  case class P(val row: Int, val col: Int) {
    def +(that: P) = row == that.row && col == that.col
  }

  case class Tile(val terrain: TileTerrain, val civ: Civilization.T, val suggestedcapital: P) {
    def civhome: Boolean = civ != null
  }

  case class Player(player: Player.T, civ: Civilization.T)

  case class City(civ: Civilization.T, citytype: City.T) {
    def defenceStrength(): Int = {
      citytype match {
        case City.Capital => 12
        case City.Normal => 6
        case City.WalledCapital => 16
        case City.WalledNormal => 10
      }
    }
  }

  object Resource extends Enumeration {
    type T = Value
    val Wheat, Silk, Incense, Iron, Coin, Spy, Uranium = Value
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
    val America, China, Egypt, Germany, Rome, Russia, Spain = Value
  }

  object City extends Enumeration {
    type T = Value
    val Capital, Normal, WalledCapital, WalledNormal = Value
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

  object Player extends Enumeration {
    type T = Value
    val A, B, C, D, E, F = Value
  }

  object TurnPhase extends Enumeration {
    type T = Value
    val StartOfTurn, Trade, CityManagement, Movement, Research = Value

    def turnAction(t: Value): Boolean = t != StartOfTurn && t != Trade
  }

  object TechnologyName extends Enumeration {
    type T = Value
    val
    Irigation,
    Construction,
    MetalCasting,
    Communism,
    MilitaryScience,
    SteamPower,
    GunPowder,
    NuclearFusion,
    ReplaceableParts,
    Ballistics,
    MessMedia,
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
    Banking = Value
  }

  def levelTrade(level: Int): Int = 6 + (level - 1) * 5
}
