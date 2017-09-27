package civilization

import java.util.Calendar

import civilization.objects._
import civilization.action.{Command, Play}


package object gameboard {

  private final val packageversion: Int = 0;

  case class PatterMap(val p: P, val o: Orientation.T)

  case class Technology(val tech: TechnologyName.T, val level: Int)

  case class PlayerTechnology(val tech: TechnologyName.T)

  case class Figures(var numberofArmies: Int, var numberofScouts: Int) {
    def empty: Boolean = (numberofArmies == 0 && numberofScouts == 0)

    def +(that: Figures) = {
      this.numberofScouts = this.numberofScouts + that.numberofScouts
      this.numberofArmies = this.numberofArmies + that.numberofArmies
    }

    def -(that: Figures) = this + Figures(0 - that.numberofArmies, 0 - that.numberofScouts)

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

  case class MapSquare(var hv: Option[HutVillage] = None, var city: Option[City] = None ) {
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
  case class MapTile(val tname: String, val p: P, var orientation: Orientation.T, val mapsquares: Array[Array[MapSquare]]) {
    var tile: Tile = _
  }

  case class BoardMap(val map: Seq[MapTile])

  case class Market(var hv: Array[HutVillage], var hvused: Seq[HutVillage])

  case class PlayerDeck(val civ: Civilization.T) {

    val defaultcitylimit: Int = 2
    val defaultarmieslimit: Int = 6
    val defaultscoutslimit: Int = 2
    val defaultculturehandsize: Int = 2
    val defaultstackinglimit: Int = 2
    val defaulttravelspeed: Int = 2
    var tech: Seq[PlayerTechnology] = Nil
  }

  case class GameMetaData(val version: Int, val createtime: Long, var accesstime: Long, val desc: String) {

    def this(desc: String) {
      this(packageversion, Calendar.getInstance().getTime.getTime, Calendar.getInstance().getTime.getTime, desc)
    }

    def okV: Boolean = version == packageversion
  }

  case class GameBoard(val players: Seq[PlayerDeck], val map: BoardMap, val market: Market) {

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
