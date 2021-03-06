package civilization

import java.util.Calendar

import civilization.objects._
import civilization.action.{Command, Play, constructCommand}
import civilization.io.readdir.GameResources
import civilization.message.J.J
import civilization.objects.Terrain.Value
import play.api.libs.json.{JsNumber, JsValue}
import civilization.gameboard.JournalElem._


/** Placeholder for objects and definitions related to the gameboard. */
package object gameboard {

  type Journal = collection.mutable.ListBuffer[JournalElem]

  type BattleArmy = Array[Option[FrontUnit]]
  /** Version: used during storing and retrieving gameboard from datastore.
    * Ignore games which does not fit to avoid runtime errors
    */
  private final val packageversion: Int = 8;

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

    def incr(r: T, num: Int = 1) = table(r) = table(r) + num

    def decr(r: T, num: Int = 1) = {
      require(table(r) >= num)
      table(r) = table(r) - num
    }

    def exist(r: T): Boolean = nof(r) > 0

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
    * @param tech    Technology name
    * @param initial Technology assigned to the player
    * @param coins   Number of coins put on the technology so far
    */
  case class PlayerTechnology(val tech: TechnologyName.T, val initial: Option[Boolean] = None) {
    private val phases: collection.mutable.Set[Int] = collection.mutable.Set()

    def coins = if (TechnologyFeatures.isCoinTechnology(tech)) phases.size else 0

    def roundAlready(roundno: Int) = phases contains roundno

    def addRound(roundno: Int) = phases += roundno

    def removeCoin() = phases.remove(phases.head)
  }

  /** Figures on the square, can be staked
    *
    * @param numberofArmies number of armies
    * @param numberofScouts number of scouts
    */
  case class Figures(var numberofArmies: Int, var numberofScouts: Int) {

    def this(f: Figure.T) =
      this(if (f == Figure.Army) 1 else 0, if (f == Figure.Scout) 1 else 0)


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

  case class CultureResources(var cards: Seq[CultureCardName.T] = Nil, var persons: Seq[GreatPersonName.T] = Nil, var personsexposed: Seq[GreatPersonName.T] = Nil)

  case class WonderSquare(val w: Wonders.T, var obsolete: Boolean)

  case class MapSquare(var hv: Option[HutVillage] = None, var city: Option[City] = None, var building: Option[Building] = None, var wonder: Option[WonderSquare] = None) {
    require(hv != null && city != null)
    val figures: PlayerFigures = new PlayerFigures(null, 0, 0)

    def hvhere: Boolean = hv.isDefined

    def cityhere: Boolean = city.isDefined

    var greatperson: Option[GreatPerson] = None
    var greatpersonype: GreatPersonType = null

    def setGreatPerson(g: GreatPersonName.T) = {
      greatperson = Some(GameResources.getGreatPerson(g))
      greatpersonype = GameResources.getGreatPersonType(greatperson.get.ptype)
    }

    def removeGreatPerson() = greatperson = None

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
    def incdecBuilding(b: BuildingName.T, inc: Boolean) = {
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

  case class PlayerDeck(val civ: Civilization.T, var tech: Seq[PlayerTechnology], var units: Seq[CombatUnit], val resou: BoardResources, var gover: GovernmentName.T, var cultureprogress: Int) {

    val defaultarmieslimit: Int = 6
    val defaultscoutslimit: Int = 2
    val defaultculturehandsize: Int = 2
    val combatlevel: CombatUnitStrength = CombatUnitStrength()
    var hvlist: Seq[HutVillage] = Nil

    def findPlayerTechnology(te: TechnologyName.T): Option[PlayerTechnology] = tech.find(_.tech == te)

    def hasTechnology(te: TechnologyName.T): Boolean = findPlayerTechnology(te).isDefined

    val cultureresource: CultureResources = CultureResources()

    def hasTechnologyFeature(feature: TechnologyName.T => Boolean): Boolean = tech.exists(te => feature(te.tech))

    def numofTechnologyFeatures(feature: TechnologyName.T => Boolean): Int = tech.filter(te => feature(te.tech)).length

    def stackLimit: Int = math.max(CivilizationFeatures.startStackingLimit(civ), if (tech.isEmpty) 0 else tech.map(te => TechnologyFeatures.stackSize(te.tech)) max)

    var freeWonder: Option[Wonders.T] = None
    // number of free resources to take, can be 3 for Navigation
    var takefreeResources: Int = 0
    // win the game
    var winthegame: Option[GameWinType.T] = None
  }

  implicit def playerDeckToCiv(deck: PlayerDeck): Civilization.T = deck.civ

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

  case class WinnerLootEffect(val name: LootEffectName.T, val loot: Int, val tech: Option[TechnologyName.T], val resource: Option[Resource.T], val cardlevel: Option[Int], val coinsheet: Option[Boolean]) {
    require(loot == 1 || loot == 2)

    def ==(v: WinnerLootEffect): Boolean = {
      if (name != v.name) return false
      if (loot != v.loot) return false
      if (tech != v.tech) return false
      if (tech.isDefined)
        if (tech.get != v.tech.get) return false
      if (cardlevel != v.cardlevel) return false
      if (cardlevel.isDefined)
        if (cardlevel.get != v.cardlevel.get) return false
      if (coinsheet != v.coinsheet) return false
      if (coinsheet.isDefined)
        if (coinsheet.get != v.coinsheet.get) return false
      if (resource != v.resource) return false
      if (resource.isDefined)
        if (resource.get != v.resource.get) return false
      true
    }

    def level2: Boolean = loot == 2
  }

  case class WinnerLoot(val loot: Int, val list: Seq[WinnerLootEffect])

  case class WondersDiscount(val cost: Int, tech: TechnologyName.T)

  case class WondersOfTheWorld(val name: Wonders.T, val phase: Option[TurnPhase.T], val age: WondersAge.T, val cost: Int, val discount: Option[WondersDiscount], val desc: String, val t: String, val notimplemented: Option[Boolean]) {
    def ni: Boolean = notimplemented.isDefined && notimplemented.get
  }

  case class FrontUnit(val unit: CombatUnit, var attackstrength: Int, var defendstrenght: Int, var wounds: Int)

  case class BattleFieldSide(val fighting: BattleArmy, var waiting: Seq[CombatUnit], var killed: Seq[CombatUnit], val strength: CombatUnitStrength, val combatBonus: Int, var canuseiron: Boolean, val isvillage: Boolean, isScouts: Boolean, var savedunit: Option[Int] = None) {
    var ironused: Int = -1

    def points: Int = {
      val su: Int = waiting.map(p => p.getStrength(strength)).sum
      su + fighting.map(f => if (f.isEmpty) 0 else f.get.attackstrength).sum + combatBonus
    }
  }

  case class BattleField(val attacker: BattleFieldSide, val defender: BattleFieldSide, val attackerciv: Civilization.T, val defenderciv: Civilization.T) {
    var attackermove: Boolean = false

    def endofbattle: Boolean = attacker.waiting.isEmpty && defender.waiting.isEmpty

    def attackerwinner: Boolean = (attacker.points > defender.points) || defender.isScouts
  }

  case class BuildingPoint(val p: P, val bui: Option[BuildingName.T], val won: Option[Wonders.T], val gp: Option[GreatPersonName.T]) {
    require(bui.isDefined && won.isEmpty && gp.isEmpty || bui.isEmpty && won.isDefined && gp.isEmpty || bui.isEmpty && won.isEmpty && gp.isDefined)

    def ==(that: BuildingPoint): Boolean =
      p == that.p &&
        (bui.isDefined && that.bui.isDefined && b == that.b ||
          (won.isDefined && that.won.isDefined && w == that.w) ||
          (gp.isDefined && that.gp.isDefined && g == that.g))

    def b = bui.get

    def w = won.get

    def g = gp.get
  }

  case class EndOfGame(val winner: Civilization.T, val wintype: GameWinType.T)

  case class ActionTypeSuspension(val civ: Civilization.T, val comm: Command.T, val par: JsValue)

  case class GameBoard(val players: Seq[PlayerDeck], val map: BoardMap, val resources: Resources, val market: Market) {

    // order of civilizations to play
    var pllist: Seq[Civilization.T] = Nil

    // action suspended
    var susplist: Seq[ActionTypeSuspension] = Nil

    def addActionSuspend(civ: Civilization.T, comm: Command.T, par: JsValue) =
      susplist = susplist :+ ActionTypeSuspension(civ, comm, par)

    def clearActionSuspend = susplist = Nil

    def suspendedForCiv(civ: Civilization.T): Seq[ActionTypeSuspension] = susplist.filter(_.civ == civ)

    def isSuspended: Boolean = !susplist.isEmpty

    def others(civ: Civilization.T): Seq[Civilization.T] = pllist.filter(civ != _)

    // cheating, for old tests only
    // do not rotate
    var norotate: Boolean = false
    // cheating, calculate trade from current data
    var tradecurrent: Boolean = false
    // cheating, test ony
    var logistricdoesnotupgradeartillery: Boolean = false

    var metadata: GameMetaData = new GameMetaData("")
    // force command to execute next
    // TODO: I'm not happy with that
    var forcednext: List[Command] = Nil
    var play: Play.Play = new Play.Play()
    var battle: Option[BattleField] = None
    val journal: Journal = collection.mutable.ListBuffer() // empty
    val cultureused: CultureResources = CultureResources()
    var endofgame: Option[EndOfGame] = None

    def rotateplorder: Unit = if (!norotate) pllist = rotaterightList(pllist)

    def addForcedCommand(com: Command) = forcednext = forcednext :+ com

    def addForcedCommandC(command: Command.T, civ: Civilization.T, p: P = null, param: JsValue = null) = {
      val commandC = action.constructCommand(command, civ, p, param)
      addForcedCommand(commandC)
    }

    def increaseTradeCommand(civ: Civilization.T, trade: Int) =
      addForcedCommandC(Command.INCREASETRADE, civ, null, JsNumber(trade))

    def increaseCultureCommand(civ: Civilization.T, culture: Int) =
      addForcedCommandC(Command.GETCULTURE, civ, null, JsNumber(culture))

    def playerDeck(civ: Civilization.T): PlayerDeck =
    // assuming exist
      players.find(p => p.civ == civ).get


    def conf: GameConfig = GameConfig(false)

    def techlevel(tep: PlayerTechnology): Int = if (tep.initial.isDefined && tep.initial.get) 1 else GameResources.getTechnology(tep.tech).level

    def getCurrentWonders(): Seq[Wonders.T] = market.wonders.take(WONDERWINDOW)

    def getTech(t: TechnologyName.T): Technology = GameResources.getTechnology(t)

    def getBuilding(b: BuildingName.T): Building = GameResources.getBuilding(b)

    def addJ(e: JournalElem) = journal += e
  }

}
