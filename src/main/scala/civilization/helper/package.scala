package civilization

import civilization.action.Command
import civilization.gameboard._
import civilization.message.{FatalError, M, Mess}
import civilization.objects._

import scala.util.control.Breaks._

package object helper {

  private val ra = scala.util.Random

  case class MapSquareP(val s: MapSquare, val sm: Square, val p: P, val t: MapTile, val suggestedCapital : Boolean) {
    def revealed: Boolean = t.orientation != null

    def terrain: Terrain.T = sm.terrain

    def numberOfTrade: Int = sm.token.numofTrade

    def numberOfProduction: Int = sm.token.numofProduction

    def resource: Resource.T = sm.resource

//    def suggestedCapital: Boolean = (t.tile.civ != null) && (t.tile.suggestedcapital == p)

    def suggestedCapitalForCiv: Civilization.T = if (suggestedCapital) t.tile.civ else null
  }

  def pointsAround(board: GameBoard, p: P): Seq[P] = {
    val all: Seq[P] = List(P(p.row - 1, p.col - 1), P(p.row - 1, p.col), P(p.row - 1, p.col + 1), P(p.row + 1, p.col - 1), P(p.row + 1, p.col), P(p.row + 1, p.col + 1), P(p.row, p.col - 1), P(p.row, p.col + 1))
    all.filter(isPointOnBoard(board, _))
  }

  def squaresAround(board: GameBoard, p: P): Seq[MapSquareP] = {
    val allp: Seq[P] = pointsAround(board, p)
    allp.map(getSquare(board, _))
  }

  def checkP(board: GameBoard, p: P) = {
    if (!isPointOnBoard(board, p)) throw FatalError(Mess(M.POINTOUTSIDEBOARD, p))
  }

  def checkCity(b: GameBoard, p: P): Option[Mess] = {
    if (getSquare(b, p).s.city == null) Some(Mess(M.NOTCITY, p))
    None
  }

  def isPointOnBoard(board: GameBoard, p: P): Boolean =
    allPoints(board).exists(_ == p)

  def allPoints(board: GameBoard): Seq[P] =
    board.map.map.flatMap(p => {
      (for (row <- 0 until TILESIZE; col <- 0 until TILESIZE) yield (P(p.p.row * TILESIZE + row, p.p.col * TILESIZE + col))) toSeq
    }) toSeq

  def allSquares(b: GameBoard): Seq[MapSquareP] =
    allPoints(b).map(getSquare(b, _)) toSeq

  def isCapitalBuild(board: GameBoard, civ: Civilization.T): Boolean =
    citiesForCivilization(board, civ).exists(p => p.s.city.citytype == City.Capital || p.s.city.citytype == City.WalledCapital)

  def citiesForCivilization(board: GameBoard, civ: Civilization.T): Seq[MapSquareP] =
    allPoints(board).map(getSquare(board, _)).filter(s => s.s.city != null && s.s.city.civ == civ).toList

  def pointtoTile(p: P): P = P(p.row / TILESIZE, p.col / TILESIZE)

  def getSquare(board: GameBoard, p: P): MapSquareP = {
    val srow: Int = p.row % TILESIZE
    val scol: Int = p.col % TILESIZE
    var row: Int = 0;
    var col: Int = 0;
    val tile: MapTile = getTile(board, pointtoTile(p))
    //    if (tile == null || !tile.revealed) return null
    if (tile == null) return null
    tile.orientation match {
      case Orientation.Left => {
        col = TILESIZE - 1 - srow
        row = scol;
      }
      case null | Orientation.Down => {
        col = scol;
        row = srow
      }
      case Orientation.Right => {
        col = srow
        row = TILESIZE - 1 - scol
      }
      case Orientation.Up => {
        col = TILESIZE - 1 - scol;
        row = TILESIZE - 1 - srow
      }
    }
    val suggestedCapital: Boolean = (tile.tile.civ != null) && (tile.tile.suggestedcapital == P(row,col))

    MapSquareP(tile.mapsquares(row)(col), tile.tile.terrain(row)(col), p, tile, suggestedCapital)
  }

  def isSquareForCity(board: GameBoard, p: P, civ:Civilization.T): Option[Mess] = {
    val s: MapSquareP = getSquare(board, p)
    if (!s.revealed) return Some(Mess(M.POINTONHIDDENTILE, p))
    if (s.terrain == Terrain.Water) return Some(Mess(M.CITYONWATER, p))
    val saround: Seq[MapSquareP] = squaresAround(board, p)
    if (saround.length != 8) return Some(Mess(M.POINTONBORDER, p))
    saround.foreach(pp => {
      if (!pp.revealed) return Some(Mess(M.POINTONHIDDENTILE, p))
      if (pp.sm.hv != null && !pp.s.hvtaken) return Some(Mess(M.POINTISBORDERINGWITHHUTORVIALLAGE, pp.p))
    }
    );
    // check other cities
    val paround: Set[P] = pointsAround(board, p).toSet
    allPoints(board).foreach(p => {
      val s: MapSquareP = getSquare(board, p)
      if (s.s.city != null) {
        val sp: Set[P] = pointsAround(board, p).toSet
        // intersection
        val common: Set[P] = sp.intersect(paround)
        if (!common.isEmpty) return Some(Mess(M.CITYISBORDERINGWITHANOTHER, common.head))
      }
    })
    paround.foreach(p => {
      val s: MapSquareP = getSquare(board, p)
      if (s.s.hv != null) return Some(Mess(M.HUTORVILLAGEATCITYOUTSKIRTS,p))
      if (!s.s.figures.empty && !s.s.figures.civOccupying(civ)) return Some(Mess(M.FOREIGNFIGURESATCITYOUTSKIRTS,p))
    })
    None
  }

  def revealTile(board: GameBoard, o: Orientation.T, p: P) {
    val m: MapTile = getTile(board, p)
    if (m.orientation != null) throw FatalError(Mess(M.TILEALREADYREVEALED, p))
    m.orientation = o
    // put huts and villages
    for (row <- 0 until m.mapsquares.length; col <- 0 until m.mapsquares(row).length)
      if (m.tile.terrain(row)(col).hv != null) m.mapsquares(row)(col).hv = getRandomHutVillage(board, m.tile.terrain(row)(col).hv)
  }

  def getTile(board: GameBoard, p: P): MapTile =
    board.map.map.find(_.p == p).get

  def getRandomHutVillage(board: GameBoard, hv: HutVillage.T): HutVillage = {
    val a: Array[Int] = board.market.hv.zipWithIndex.filter(p => p._1.hv == hv).map(p => p._2).toList.toArray
    // throws if empty, unexpected here
    //a.foreach(println)
    if (a.isEmpty) throw FatalError(Mess(M.NOUNUSEDHUTORVILLAGES, hv))
    // generate random
    val i: Int = ra.nextInt(a.length)
    // index in used
    val pos: Int = a(i)
    val hvreturn: HutVillage = board.market.hv(pos)
    // remove from used
    // convert to ArrayBuffer
    // TODO: very bad, replace hv type with mutable.Buffer
    val buf = board.market.hv.toBuffer
    buf.remove(pos)
    // convert to buffer again
    board.market.hv = buf.toArray
    hvreturn
  }

  //def playList(b: GameBoard): Unit = playList(b, b.play)

  case class CurrentPhase(val notcompleted: Seq[Civilization.T], val turnPhase: TurnPhase.T, val roundno: Int)

  private def getPhase(c: Command): Option[TurnPhase.T] = if (c.command == Command.ENDOFPHASE) Some(c.param.asInstanceOf[TurnPhase.T]) else None

  private def allCivs(b: GameBoard): Seq[Civilization.T] = b.players.map(_.civ).toSeq

  private def nextPhase(pha: TurnPhase.T): TurnPhase.T = if (pha == TurnPhase.Research) TurnPhase.StartOfTurn else TurnPhase.apply(pha.id + 1)

  private def prevPhase(pha: TurnPhase.T): TurnPhase.T = if (pha == TurnPhase.StartOfTurn) TurnPhase.Research else TurnPhase.apply(pha.id - 1)

  def currentPhase(b: GameBoard): CurrentPhase = {

    // reverse command, analyze from end
    val rlist: Seq[action.Command] = b.play.commands.reverse
    // collect civ for last phase
    // filter only ENDOFPHASE
    val ephases: Seq[Command] = rlist.filter(getPhase(_).isDefined)

    // beginning of game
    if (ephases.isEmpty) return CurrentPhase(allCivs(b), TurnPhase.StartOfTurn, 0)

    var players: Set[Civilization.T] = allCivs(b).toSet
    // calculate roundno
    // complicated but working
    // full single round if StartOfTurn less or equal number of players, so the 1 is deducted
    // during test can be less then 0 if there is no StartOfTurn
    val roundno = math.max((ephases.filter(getPhase(_).get == TurnPhase.StartOfTurn).length - 1) / players.size, 0)


    // current phase
    val currentphase: TurnPhase.T = getPhase(ephases.head).get
    // collect civs not completing current phase
    breakable {
      ephases.foreach(c => {
        if (getPhase(c).get != currentphase) break
        players = players - c.civ
      }
      )
    }
    // not empty, current phase is executed, prepare list of active players in order, remove completed
    if (!players.isEmpty) return CurrentPhase(allCivs(b).filter(c => players(c)), currentphase, roundno)
    // empty, next phase
    CurrentPhase(allCivs(b), nextPhase(currentphase), if (currentphase == TurnPhase.Research) roundno + 1 else roundno)
  }

  private def lastPhaseCommandsReverse(b: GameBoard, civ: Civilization.T, pha: TurnPhase.T): Seq[Command] = {
    val rlist: Seq[Command] = b.play.commands.reverse
    var clist: Seq[Command] = Nil
    breakable {
      rlist.foreach(c => {
        if (getPhase(c) != None && getPhase(c).get != pha) break
        if (c.civ == civ) clist = clist :+ c
      })
    }
    clist
  }

  def CityAvailableForAction(b: GameBoard, civ: Civilization.T): Seq[P] = {
    val p: Seq[Command] = lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement)
    // all cities
    var cities: Set[P] = citiesForCivilization(b, civ).map(_.p).toSet
    p.foreach(c => cities = cities - c.p)
    cities.toSeq
  }

  case class Move(val command: Command.T, val p: Option[P])

  case class PlayerMove(val f: PlayerFigures, val moves: Seq[Move]) {
    def begstop: (Move, Move) = (moves.head, moves.reverse.find(_.p.isDefined).get)

    def len: Int = moves.filter(_.p.isDefined).length - 1

    def isFinished = moves.last.command == Command.ENDOFMOVE

    def lastp: P = begstop._2.p.get
  }

  def civLastMoves(b: GameBoard, civ: Civilization.T): Seq[PlayerMove] = {
    val cu: CurrentPhase = currentPhase(b)
    if (cu.turnPhase != TurnPhase.Movement) return Nil
    if (cu.notcompleted.head != civ) return Nil
    // proper order again
    val p: Seq[Command] = lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement).reverse
    // start collecting moves
    var li: Seq[PlayerMove] = Nil
    var fig: PlayerFigures = null
    // if null then Reveal
    var moves: Seq[Move] = Nil
    p.foreach(co => co.command match {
      case Command.MOVE | Command.ENDOFMOVE =>
        moves = moves :+ Move(co.command, if (co.p == null) None else Some(co.p))
      case Command.REVEALTILE => moves = moves :+ moves.last // for reveal repeat last, figure is stanng
      case _ => {
        if (fig != null) li = li :+ PlayerMove(fig, moves)
        fig = null
        moves = Nil
        if (co.command == Command.STARTMOVE) {
          val f: Figures = co.param.asInstanceOf[Figures]
          fig = PlayerFigures(co.civ, f.numberofArmies, f.numberofScouts)
          moves = moves :+ Move(co.command, if (co.p == null) None else Some(co.p))
        }
      }
    })
    if (fig != null) li = li :+ PlayerMove(fig, moves)
    li
  }


  def getCurrentMove(b: GameBoard, civ: Civilization.T): Option[PlayerMove] = {
    val l: Seq[PlayerMove] = civLastMoves(b, civ)
    if (l.isEmpty) None else Some(l.last)
  }

  private def commandForPhase(b: GameBoard, command: Command): Mess = {
    val current: CurrentPhase = currentPhase(b)
    val phase: TurnPhase.T = Command.actionPhase(command.command)
    if (phase != null && phase != current.turnPhase) return Mess(M.ACTIONCANNOTBEEXECUTEDINTHISPHASE, (command, current.turnPhase, phase))
    if (phase != null && phase == TurnPhase.CityManagement) {
      // check if city correct
      val ss: MapSquareP = getSquare(b, command.p)
      if (ss.s.city == null || ss.s.city.civ != command.civ) return (Mess(M.THEREISNOCIVLIZATIONCITYATTHISPOINT, (command)))
      // duplicate CityManagement for the same city
      val cities: Seq[P] = CityAvailableForAction(b, command.civ)
      if (cities.find(_ == command.p).isEmpty) return Mess(M.DUPLICATECITYACTIONINTHISCITY, (command))
    }
    // check if end of move setup correctly
    if (phase == TurnPhase.Movement) {
      val moo: Option[PlayerMove] = getCurrentMove(b, command.civ)
      // TODO: improve
      if (moo.isDefined) {
        val mo: PlayerMove = moo.get
        if ((!Command.actionMove(command.command) && mo.f.civ == command.civ) && !mo.isFinished) return Mess(M.LASTMOVENOTENDED, mo.moves.head)
      }
    }
    // check if only one research in turn
    if (phase == TurnPhase.Research && current.turnPhase == TurnPhase.Research) {
      val p: Seq[Command] = lastPhaseCommandsReverse(b, command.civ, TurnPhase.Research)
      if (!p.isEmpty) return Mess(M.CANNOTRESEARCHMORETHENONCEINSINGLETURN, command)
    }
    null
  }

  def playsingleCommand(b: GameBoard, command: Command, f: Command => Unit = p => Unit): Mess = {
    var m: Mess = commandForPhase(b, command)
    if (m != null) return m
    // test if point on board
    if (command.p != null && !isPointOnBoard(b, command.p)) return Mess(M.POINTOUTSIDEBOARD, command.p)
    m = command.verify(b)
    if (m != null) {
      return m
    }
    command.execute(b)
    f(command)
    b.play.commands = b.play.commands :+ command
    null
  }

  def playCommand(b: GameBoard, command: Command, f: Command => Unit = p => Unit): Mess = {
    var m: Mess = playsingleCommand(b, command, f)
    if (m != null) return m
    // play forced commands
    while (!b.forcednext.isEmpty) {
      val com: Command = b.forcednext.head
      b.forcednext = b.forcednext.tail
      m = playsingleCommand(b, com, f)
      if (m != null) return m
    }
    null
  }

  def gameStart(b: GameBoard): Boolean = currentPhase(b).roundno == 0

  def numberofTrade(b: GameBoard, civ: Civilization.T): Integer = {
    val num: Int = citiesForCivilization(b, civ).flatMap(p => squaresAround(b, p.p)).map(_.numberOfTrade).foldLeft(0)(_ + _)
    if (gameStart(b)) num * 2 else num
  }


  def getProductionForCity(b: GameBoard, p: P): Integer = {
    val num: Int = squaresAround(b, p).map(s => getSquare(b, s.p).numberOfProduction).foldLeft(0)(_ + _)
    return num
  }

  case class PlayerLimits(val citieslimit: Int, val stackinglimit: Integer, val watercrossingallowed: Boolean, val waterstopallowed: Boolean, val armieslimit: Int, val scoutslimit: Int, val travelSpeed: Int)

  def getLimits(b: GameBoard, civ: Civilization.T): PlayerLimits = {
    val deck: PlayerDeck = b.playerDeck(civ)
    val citieslimit: Int = deck.defaultcitylimit - citiesForCivilization(b, civ).length
    val count: (Int, Int) = getNumberOfArmies(b, civ)
    val armieslimit: Int = deck.defaultarmieslimit - count._1
    val scoutslimit: Int = deck.defaultscoutslimit - count._2
    PlayerLimits(citieslimit, deck.defaultstackinglimit, false, false, armieslimit, scoutslimit, deck.defaulttravelspeed)

  }

  def getFigures(b: GameBoard, civ: Civilization.T): Seq[MapSquareP] = allSquares(b).filter(s => s.s.figures.civOccupying(civ))

  def getNumberOfArmies(b: GameBoard, civ: Civilization.T): (Int, Int) =
    getFigures(b, civ).foldLeft((0, 0))((s1, s2) => (s1._1 + s2.s.figures.numberofArmies, s1._2 + s2.s.figures.numberofScouts))

  def isSquareForFigure(b: GameBoard, civ: Civilization.T, f: Figure.T, p: P): Option[Mess] = {
    val li: PlayerLimits = getLimits(b, civ)
    val count: (Int, Int) = getNumberOfArmies(b, civ)
    f match {
      case Figure.Army => if (li.armieslimit < 1) return Some(Mess(M.LIMITFORARMIESEXCEEDED, (count._1, li.armieslimit)))
      case Figure.Scout => if (li.scoutslimit < 1) return Some(Mess(M.LIMITFORSCOUTSEXCEEDED, (count._2, li.scoutslimit)))
    }
    val s: MapSquareP = getSquare(b, p)
    if (s.s.city != null) return Some(Mess(M.CANNOTSETFIGUREONCITY, p))
    if (s.sm.terrain == Terrain.Water && !li.waterstopallowed) return Some(Mess(M.CANNOTPUTFIGUREONWATER, p))
    if (s.s.figures.civ == null) return None
    if (f == Figure.Scout && s.s.figures.numberofScouts > 0) return Some(Mess(M.ONLYONESCIUTALLOWED, p))
    if (s.s.figures.numberofScouts + s.s.figures.numberofArmies + 1 > li.stackinglimit) return Some(Mess(M.STACKINGSIZEEXCEEDED, p))
    None
  }

  def canBuyFigure(b: GameBoard, civ: Civilization.T, p: P, f: Figure.T): Option[Mess] = {
    val cost: Int = ObjectCost.getCost(f)
    val prod: Int = getProductionForCity(b, p)
    if (cost > prod) Some(Mess(M.CANNOTAFFORDOBJECT, (f, cost, prod))) else None
  }

  // p1 is looking to p2
  // e.g (1,1) -> (1,2) : Right
  def getOrientation(p1: P, p2: P): Orientation.T = {
    if (p1.row == p2.row)
      return if (p2.col > p1.col) Orientation.Right else Orientation.Left
    if (p2.row > p1.row) Orientation.Up else Orientation.Down
  }

  // move figures

  def putFigures(b: GameBoard, civ: Civilization.T, p: P, f: Figures) = {
    val s: MapSquareP = getSquare(b, p)
    s.s.figures.civ = civ
    s.s.figures + f
  }

}
