package civilization

import java.util.Calendar

import civilization.objects._
import civilization.action.{Command, Play}


/** Placeholder for objects and definitions related to the gameboard. */
package object gameboard {

  /** Version: used during storing and retrieving gameboard from datastore.
    * Ignore games which does not fit to avoid runtime errors
    */
  private final val packageversion: Int = 2;

  /** Part of map pattern. Position of the tile and orientation.
    * Initially only civilication tiles have orientation specified.
    * The tile is revealed if o isDefine  otherwise still hidden
    * @param p Position in the map pattern, row and column, (0,0) means (bottom, left)
    * @param o Orienation. If isDefine the tile is revealed
    */
  case class PatternMap(val p: P, val o: Option[Orientation.T])


  /** Technology dictionary
    *
    * @param tech  Technology name
    * @param level Level of this technology
    */
  case class Technology(val tech: TechnologyName.T, val level: Int)

  /** TODO : consider
    * @param tech
    */
  case class PlayerTechnology(val tech: TechnologyName.T)

  /** Figures on the square, can be staked
    *
    * @param numberofArmies number of armies
    * @param numberofScouts number of scouts
    */
  case class Figures(var numberofArmies: Int, var numberofScouts: Int) {
    /** Not occupied by any army */
    def empty: Boolean = (numberofArmies == 0 && numberofScouts == 0)

    /** Adds figures, move figures over stationing */
    def +(that: Figures) = {
      this.numberofScouts = this.numberofScouts + that.numberofScouts
      this.numberofArmies = this.numberofArmies + that.numberofArmies
    }

    /** Minus, move figures from squre */
    def -(that: Figures) = this + Figures(0 - that.numberofArmies, 0 - that.numberofScouts)

    /** Minus, auxiliary */
    def unary_- : Figures = Figures(0 - numberofArmies, 0 - numberofScouts)
  }

  case class PlayerFigures(var civ: Civilization.T, var numberofArmies: Int, var numberofScouts: Int) {

    require(ok)

    private def ok: Boolean = numberofArmies >= 0 && numberofScouts >= 0

    def empty: Boolean = (numberofArmies == 0 && numberofScouts == 0)

    def civOccupying(civ: Civilization.T): Boolean = !empty && this.civ == civ

    def +(that: Figures) = {
      this.numberofScouts = this.numberofScouts + that.numberofScouts
      this.numberofArmies = this.numberofArmies + that.numberofArmies
      if (empty) civ = null
      require(ok)
    }

    def kill() = {
      this.numberofScouts = 0
      this.numberofArmies = 0
      civ = null
    }

    def toFigures: Figures = Figures(numberofArmies, numberofScouts)
  }

  case class MapSquare(var hv: Option[HutVillage] = None, var city: Option[City] = None) {
    require(hv != null && city != null)
    val figures: PlayerFigures = new PlayerFigures(null, 0, 0)

    def hvhere: Boolean = hv.isDefined

    def cityhere: Boolean = city.isDefined
  }

  def genEmptySquares: Array[Array[MapSquare]] = {
    val squares: Array[Array[MapSquare]] = Array.ofDim(TILESIZE, TILESIZE)
    for (i <- 0 until TILESIZE; j <- 0 until TILESIZE) squares(i)(j) = MapSquare()
    squares
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

  class GameResources {

    //    val table : scala.collection.mutable.Map[Resource.T,Int] = Resource.values.map(v => (v,0)).toMap
    val table: scala.collection.mutable.Map[Resource.T, Int] = scala.collection.mutable.Map(
      Resource.Coin -> 0, Resource.Spy -> 0, Resource.Silk -> 0, Resource.Incense -> 0,
      Resource.Iron -> 0, Resource.Uranium -> 0, Resource.Wheat -> 0)

    def setResNum(r: Resource.T, num: Int) = table.put(r, num)
    def incr(r:Resource.T) = table(r) = table(r) + 1
    def decr(r:Resource.T) = {
      require(table(r) > 0)
      table(r) = table(r) - 1
    }
    def nof(r : Resource.T) : Int = table(r)

  }

  case class Resources(var hv: Seq[HutVillage], var hvused: Seq[HutVillage], val resou: GameResources)

  case class PlayerDeck(val civ: Civilization.T, var tech: Seq[PlayerTechnology], var units: Seq[CombatUnit],val resou: GameResources) {

    val defaultcitylimit: Int = 2
    val defaultarmieslimit: Int = 6
    val defaultscoutslimit: Int = 2
    val defaultculturehandsize: Int = 2
    val defaultstackinglimit: Int = 2
    val defaulttravelspeed: Int = 2
    val combatlevel: CombatUnitStrength = CombatUnitStrength()
    var hvlist: Seq[HutVillage] = Nil
  }

  case class Market(var units: Seq[CombatUnit], var killedunits: Seq[CombatUnit])

  case class GameMetaData(val version: Int, val createtime: Long, var accesstime: Long, val desc: String) {

    def this(desc: String) {
      this(packageversion, Calendar.getInstance().getTime.getTime, Calendar.getInstance().getTime.getTime, desc)
    }

    def okV: Boolean = version == packageversion
  }

  case class GameBoard(val players: Seq[PlayerDeck], val map: BoardMap, val resources: Resources, val market: Market) {

    var metadata: GameMetaData = new GameMetaData("")

    // force command to execute next
    // TODO: I'm not happy with that
    var forcednext: List[Command] = Nil

    def addForcedCommand(com: Command) = forcednext = forcednext :+ com

    def playerDeck(civ: Civilization.T): PlayerDeck = {
      // assuming exist
      players.find(p => p.civ == civ).get
    }

    var play: Play.Play = new Play.Play()
    var tech: Seq[Technology] = _
  }


}
