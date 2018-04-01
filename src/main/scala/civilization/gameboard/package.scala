package civilization

import java.util.Calendar

import civilization.objects._
import civilization.action.{Command, Play}
import civilization.io.readdir.GameResources


/** Placeholder for objects and definitions related to the gameboard. */
package object gameboard {

  type BattleArmy = Array[Option[FrontUnit]]
  /** Version: used during storing and retrieving gameboard from datastore.
    * Ignore games which does not fit to avoid runtime errors
    */
  private final val packageversion: Int = 5;

  def genEmptySquares: Array[Array[MapSquare]] = {
    val squares: Array[Array[MapSquare]] = Array.ofDim(TILESIZE, TILESIZE)
    for (i <- 0 until TILESIZE; j <- 0 until TILESIZE) squares(i)(j) = MapSquare()
    squares
  }

  def eqO[T](p1: Option[T], p2: Option[T]): Boolean = {
    if (p1.isEmpty && p2.isEmpty) return true
    if (p1.isEmpty) return false
    if (p2.isEmpty) return false
    return p1.get == p2.get
  }

  private def rotaterightList[T](l: Seq[T]): Seq[T] = if (l.isEmpty) l else l.tail :+ l.head

  trait EnumResources[T] {

    val table: scala.collection.mutable.Map[T, Int] = scala.collection.mutable.Map()

    def setResNum(r: T, num: Int) = table.put(r, num)

    def incr(r: T) = table(r) = table(r) + 1

    def decr(r: T) = {
      require(table(r) > 0)
      table(r) = table(r) - 1
    }

    def nof(r: T): Int = table(r)
  }

  /** Part of map pattern. Position of the tile and orientation.
    * Initially only civilication tiles have orientation specified.
    * The tile is revealed if o isDefine  otherwise still hidden
    *
    * @param p Position in the map pattern, row and column, (0,0) means (bottom, left)
    * @param o Orienation. If isDefine the tile is revealed
    */
  case class PatternMap(val p: P, val o: Option[Orientation.T])

  /** TODO : consider
    *
    * @param tech
    */
  case class PlayerTechnology(val tech: TechnologyName.T, val initial: Option[Boolean] = None, var coins : Option[Int] = None)

  /** Figures on the square, can be staked
    *
    * @param numberofArmies number of armies
    * @param numberofScouts number of scouts
    */
  case class Figures(var numberofArmies: Int, var numberofScouts: Int) {
    /** Not occupied by any army */
    def empty: Boolean = (numberofArmies == 0 && numberofScouts == 0)

    /** Minus, move figures from squre */
    def -(that: Figures) = this + Figures(0 - that.numberofArmies, 0 - that.numberofScouts)

    /** Adds figures, move figures over stationing */
    def +(that: Figures) = {
      this.numberofScouts = this.numberofScouts + that.numberofScouts
      this.numberofArmies = this.numberofArmies + that.numberofArmies
    }

    /** Minus, auxiliary */
    def unary_- : Figures = Figures(0 - numberofArmies, 0 - numberofScouts)
  }

  case class PlayerFigures(var civ: Civilization.T, var numberofArmies: Int, var numberofScouts: Int) {

    require(ok)

    def civOccupying(civ: Civilization.T): Boolean = !empty && this.civ == civ

    def +(that: Figures) = {
      this.numberofScouts = this.numberofScouts + that.numberofScouts
      this.numberofArmies = this.numberofArmies + that.numberofArmies
      if (empty) civ = null
      require(ok)
    }

    private def ok: Boolean = numberofArmies >= 0 && numberofScouts >= 0

    def empty: Boolean = (numberofArmies == 0 && numberofScouts == 0)

    def kill() = {
      this.numberofScouts = 0
      this.numberofArmies = 0
      civ = null
    }

    def toFigures: Figures = Figures(numberofArmies, numberofScouts)
  }

  case class WonderSquare(val w: Wonders.T, var obsolete: Boolean)

  case class MapSquare(var hv: Option[HutVillage] = None, var city: Option[City] = None, var building: Option[Building] = None, var wonder: Option[WonderSquare] = None) {
    require(hv != null && city != null)
    val figures: PlayerFigures = new PlayerFigures(null, 0, 0)

    def hvhere: Boolean = hv.isDefined

    def cityhere: Boolean = city.isDefined


    def setBuilding(b: BuildingName.T) = building = Some(GameResources.getBuilding(b))

    def removeBuilding() = building = None
  }

  // tile is var, enriched later by tname
  case class MapTile(val tname: String, val p: P, var orientation: Option[Orientation.T], val mapsquares: Array[Array[MapSquare]]) {
    assert(orientation != null)
    assert(orientation != Some(null))
    var tile: Tile = _
  }

  case class BoardMap(val map: Seq[MapTile]) {
    // optimization, gen all points
    val allpoints: Seq[P] = genAllPoints
    val setallpoints: Set[P] = allpoints.toSet
    val pointsaround: Map[P, Seq[P]] = genpointsAround

    private def genpointsAround: Map[P, Seq[P]] =
      allpoints.map(p => (p -> pointsAround(p))) toMap

    private def pointsAround(p: P): Seq[P] = {
      val all: Seq[P] = List(P(p.row - 1, p.col - 1), P(p.row - 1, p.col), P(p.row - 1, p.col + 1), P(p.row + 1, p.col - 1), P(p.row + 1, p.col), P(p.row + 1, p.col + 1), P(p.row, p.col - 1), P(p.row, p.col + 1))
      all.filter(setallpoints.contains(_))
    }

    private def genAllPoints: Seq[P] =
      map.flatMap(p =>
        (for (row <- 0 until TILESIZE; col <- 0 until TILESIZE) yield (P(p.p.row * TILESIZE + row, p.p.col * TILESIZE + col))) toSeq
      )

  }

  class BoardResources extends EnumResources[Resource.T] {
    override val table: scala.collection.mutable.Map[Resource.T, Int] = scala.collection.mutable.Map(
      Resource.Coin -> 0, Resource.Spy -> 0, Resource.Silk -> 0, Resource.Incense -> 0,
      Resource.Iron -> 0, Resource.Uranium -> 0, Resource.Wheat -> 0, Resource.Culture -> 0)
  }

  class BuildingsResources extends EnumResources[BuildingName.T] {
    override val table: scala.collection.mutable.Map[BuildingName.T, Int] = scala.collection.mutable.Map(
      BuildingName.Shipyard -> 0, BuildingName.Harbor -> 0, BuildingName.TradingPost -> 0,
      BuildingName.Barracks -> 0, BuildingName.Workshop -> 0, BuildingName.Granary -> 0,
      BuildingName.Library -> 0, BuildingName.Temple -> 0, BuildingName.Market -> 0)

    private def toBaseBuilding(bpar: BuildingName.T): BuildingName.T = {
      var bd: BuildingName.T = bpar;
      GameResources.instance().buldings.foreach(b => {
        // converts upgraded to basic
        if (b.upgrade.isDefined && b.upgrade.get == bpar) bd = b.name;
      }
      )
      bd
    }

    /**
      * increases/decreases the building number in the market
      * converts upgraded building to basic building
      *
      * @param b   Building
      * @param inc increase or decrease
      */
    def incdevBuilding(b: BuildingName.T, inc: Boolean) = {
      var bd: BuildingName.T = toBaseBuilding(b)
      require(inc || nof(bd) > 0)
      if (inc) incr(bd)
      else decr(bd)
    }

    /**
      * number of buildings in the market
      *
      * @param b Building, base and upgraded are the same here
      * @return number of buldings
      */
    def noB(b: BuildingName.T): Int = nof(toBaseBuilding(b))
  }

  case class Resources(var hv: Seq[HutVillage], var hvused: Seq[HutVillage], val resou: BoardResources)

  case class PlayerDeck(val civ: Civilization.T, var tech: Seq[PlayerTechnology], var units: Seq[CombatUnit], val resou: BoardResources, var gover: GovernmentName.T) {

    val defaultcitylimit: Int = 2
    val defaultarmieslimit: Int = 6
    val defaultscoutslimit: Int = 2
    val defaultculturehandsize: Int = 2
    val defaultstackinglimit: Int = 2
    val defaulttravelspeed: Int = 2
    val combatlevel: CombatUnitStrength = CombatUnitStrength()
    var hvlist: Seq[HutVillage] = Nil
  }

  case class Market(var units: Seq[CombatUnit], var killedunits: Seq[CombatUnit], val buildings: BuildingsResources, var wonders: Seq[Wonders.T])

  case class GameMetaData(val version: Int, val createtime: Long, var accesstime: Long, var modiftimemili: Long, val desc: String) {

    def this(desc: String) {
      this(packageversion, Calendar.getInstance().getTime.getTime, Calendar.getInstance().getTime.getTime, Calendar.getInstance().getTimeInMillis, desc)
    }

    def modiftimestamp(): Unit = {
      modiftimemili = Calendar.getInstance().getTimeInMillis
    }

    def okV: Boolean = version == packageversion
  }

  case class WinnerLoot(val hv: Option[HutVillage.T], val res: Option[Resource.T], val trade: Boolean, val culture: Boolean) {
    def ==(v: WinnerLoot): Boolean = {
      if (trade != v.trade) return false
      if (culture != v.culture) return false
      return eqO(hv, v.hv) && eqO(res, v.res)
    }

    def noloot(): Boolean = {
      return !trade && !culture && hv.isEmpty && res.isEmpty
    }
  }

  case class WondersDiscount(val cost: Int, tech: TechnologyName.T)

  case class WondersOfTheWorld(val name: Wonders.T, val phase: Option[TurnPhase.T], val age: WondersAge.T, val cost: Int, val discount: Option[WondersDiscount], val desc: String, val t: String, val notimplemented: Option[Boolean]) {
    def ni : Boolean = notimplemented.isDefined && notimplemented.get
  }

  case class TakeWinnerLoot(val winner: Civilization.T, val loser: Civilization.T, val loot: WinnerLoot, val reso: Option[Resource.T], val trade: Int)

  case class FrontUnit(val unit: CombatUnit, var attackstrength: Int, var defendstrenght: Int, var wounds: Int)

  case class BattleFieldSide(val fighting: BattleArmy, var waiting: Seq[CombatUnit], var killed: Seq[CombatUnit], val strength: CombatUnitStrength, val combatBonus: Int, var canuseiron: Boolean, val isvillage: Boolean) {
    var ironused: Int = -1

    def points: Int = {
      val su: Int = waiting.map(p => p.getStrength(strength)).sum
      su + fighting.map(f => if (f.isEmpty) 0 else f.get.attackstrength).sum + combatBonus
    }
  }

  case class BattleField(val attacker: BattleFieldSide, val defender: BattleFieldSide, val attackerciv: Civilization.T, val defenderciv: Civilization.T) {
    var attackermove: Boolean = false

    def endofbattle: Boolean = attacker.waiting.isEmpty && defender.waiting.isEmpty

    def attackerwinner: Boolean = attacker.points > defender.points
  }

  case class BuildingPoint(val p: P, val bui: Option[BuildingName.T], val won: Option[Wonders.T]) {
    require(bui.isDefined && won.isEmpty || bui.isEmpty && won.isDefined)

    def ==(that: BuildingPoint): Boolean =
      p == that.p &&
        (bui.isDefined && that.bui.isDefined && bui.get == that.bui.get ||
          (won.isDefined && that.won.isDefined && won.get == that.won.get))

    def b = bui.get

    def w = won.get
  }

  case class GameBoard(val players: Seq[PlayerDeck], val map: BoardMap, val resources: Resources, val market: Market) {

    // order of civilizations to play
    var pllist: Seq[Civilization.T] = Nil

    // cheating, for old tests only
    // do not rotate
    var norotate: Boolean = false
    // cheating, calculate trade from current data
    var tradecurrent : Boolean = false
    var metadata: GameMetaData = new GameMetaData("")
    // force command to execute next
    // TODO: I'm not happy with that
    var forcednext: List[Command] = Nil
    var play: Play.Play = new Play.Play()
    var battle: Option[BattleField] = None

    def rotateplorder: Unit = if (!norotate) pllist = rotaterightList(pllist)

    def addForcedCommand(com: Command) = forcednext = forcednext :+ com

    def playerDeck(civ: Civilization.T): PlayerDeck = {
      // assuming exist
      players.find(p => p.civ == civ).get
    }

    def conf: GameConfig = GameConfig(false)

    def techlevel(tep: PlayerTechnology): Int = if (tep.initial.isDefined && tep.initial.get) 1 else GameResources.getTechnology(tep.tech).level

    def getCurrentWonders(): Seq[Wonders.T] = market.wonders.take(WONDERWINDOW)

    def getTech(t : TechnologyName.T) : Technology = GameResources.getTechnology(t)

    def getBuilding(b : BuildingName.T) : Building = GameResources.getBuilding(b)
  }


}
