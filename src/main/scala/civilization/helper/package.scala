package civilization

import civilization.action.Command
import civilization.gameboard._
import civilization.message.{FatalError, M, Mess}
import civilization.objects._

import scala.util.control.Breaks._

package object helper {

  private val ra = scala.util.Random

  case class MapSquareP(val s: MapSquare, val sm: Square, val p: P, val t: MapTile, val suggestedCapital: Boolean) {
    def revealed: Boolean = t.orientation.isDefined

    def terrain: Terrain.T = sm.terrain

    def numberOfTrade: Int =
    // if building trade from building
      if (s.building.isDefined) s.building.get.tokens.numofTrade
      else sm.token.numofTrade

    def numberOfProduction: Int =
    // if building, production from building
      if (s.building.isDefined) s.building.get.tokens.numofProduction
      else sm.token.numofProduction

    def resource: Option[Resource.T] =
    // building covers resource
      if (s.building.isDefined) None
      else sm.resource

    def suggestedCapitalForCiv: Option[Civilization.T] = if (suggestedCapital) Some(t.tile.civ) else None

    def civHere: Option[Civilization.T] = if (s.cityhere) Some(s.city.get.civ) else if (!s.figures.empty) Some(s.figures.civ) else None
  }

  def pointsAround(board: GameBoard, p: P): Seq[P] = board.map.pointsaround.get(p).get

  def squaresAround(board: GameBoard, p: P): Seq[MapSquareP] = {
    val allp: Seq[P] = pointsAround(board, p)
    allp.map(getSquare(board, _))
  }

  def checkP(board: GameBoard, p: P) =
    if (!isPointOnBoard(board, p)) throw FatalError(Mess(M.POINTOUTSIDEBOARD, p))

  def checkCity(b: GameBoard, p: P): Option[Mess] = {
    if (!getSquare(b, p).s.cityhere) Some(Mess(M.NOTCITY, p))
    None
  }

  def isPointOnBoard(board: GameBoard, p: P): Boolean = board.map.setallpoints.contains(p)

  def allPoints(board: GameBoard): Seq[P] = board.map.allpoints

  def allSquares(b: GameBoard): Seq[MapSquareP] =
    allPoints(b).map(getSquare(b, _))

  def isCapitalBuild(board: GameBoard, civ: Civilization.T): Boolean =
    citiesForCivilization(board, civ).exists(p => p.s.city.get.citytype == City.Capital || p.s.city.get.citytype == City.WalledCapital)

  def citiesForCivilization(board: GameBoard, civ: Civilization.T): Seq[MapSquareP] =
    allPoints(board).map(getSquare(board, _)).filter(s => s.s.cityhere && s.s.city.get.belongsTo(civ)).toList

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
      case Some(Orientation.Left) => {
        col = TILESIZE - 1 - srow
        row = scol;
      }
      case None | Some(Orientation.Down) => {
        col = scol;
        row = srow
      }
      case Some(Orientation.Right) => {
        col = srow
        row = TILESIZE - 1 - scol
      }
      case Some(Orientation.Up) => {
        col = TILESIZE - 1 - scol;
        row = TILESIZE - 1 - srow
      }
    }
    val suggestedCapital: Boolean = (tile.tile.civ != null) && (tile.tile.suggestedcapital == P(row, col))

    MapSquareP(tile.mapsquares(row)(col), tile.tile.terrain(row)(col), p, tile, suggestedCapital)
  }

  def isSquareForCity(board: GameBoard, p: P, civ: Civilization.T): Option[Mess] = {
    val s: MapSquareP = getSquare(board, p)
    if (!s.revealed) return Some(Mess(M.POINTONHIDDENTILE, p))
    if (s.terrain == Terrain.Water) return Some(Mess(M.CITYONWATER, p))
    val saround: Seq[MapSquareP] = squaresAround(board, p)
    if (saround.length != 8) return Some(Mess(M.POINTONBORDER, p))
    saround.foreach(pp => {
      if (!pp.revealed) return Some(Mess(M.POINTONHIDDENTILE, p))
      if (pp.s.hvhere) return Some(Mess(M.POINTISBORDERINGWITHHUTORVIALLAGE, pp.p))
    }
    )
    // check other cities
    val paround: Set[P] = pointsAround(board, p).toSet
    allPoints(board).foreach(p => {
      val s: MapSquareP = getSquare(board, p)
      if (s.s.cityhere) {
        val sp: Set[P] = pointsAround(board, p).toSet
        // intersection
        val common: Set[P] = sp.intersect(paround)
        if (!common.isEmpty) return Some(Mess(M.CITYISBORDERINGWITHANOTHER, common.head))
      }
    })
    paround.foreach(p => {
      val s: MapSquareP = getSquare(board, p)
      if (s.s.hvhere) return Some(Mess(M.HUTORVILLAGEATCITYOUTSKIRTS, p))
      if (!s.s.figures.empty && !s.s.figures.civOccupying(civ)) return Some(Mess(M.FOREIGNFIGURESATCITYOUTSKIRTS, p))
    })
    None
  }

  def revealTile(board: GameBoard, o: Orientation.T, p: P) {
    val m: MapTile = getTile(board, p)
    if (m.orientation.isDefined) throw FatalError(Mess(M.TILEALREADYREVEALED, p))
    m.orientation = Some(o)
  }

  def getTile(board: GameBoard, p: P): MapTile =
    board.map.map.find(_.p == p).get

  def getRandomHutVillage(board: GameBoard, hv: HutVillage.T): HutVillage = {
    val a: Seq[HutVillage] = board.resources.hv.filter(_.hv == hv)
    // throws if empty, unexpected here
    if (a.isEmpty) throw FatalError(Mess(M.NOUNUSEDHUTORVILLAGES, hv))
    val removed = getRandom(a, 1)
    //    val removed = getRandomRemove[HutVillage](a)
    board.resources.hv = board.resources.hv.filter(_.hv != hv) ++ removed._2
    removed._1.head
  }


  // ==============================
  // CombatUnits handling
  // ==============================


  def getRemove[T](a: Seq[T], i: Int): (T, Seq[T]) = {
    val b = a.toBuffer
    val t: T = b(i)
    b.remove(i)
    return (t, b)
  }

  def getRandom[T](a: Seq[T], no: Integer): (Seq[T], Seq[T]) = {
    // mutables
    val b = a.toBuffer
    val res: scala.collection.mutable.ArrayBuffer[T] = scala.collection.mutable.ArrayBuffer.empty
    for (i <- 0 until no) {
      val randI: Int = ra.nextInt(b.size)
      //
      res += b(randI)

      b.remove(randI)
    }
    // convert again to immutables
    (res, b)
  }

  def removeFromSeq[T](u: Seq[T], toremove: Seq[T], eq: (T, T) => Boolean): Seq[T] = {
    // mutable
    val b = toremove.toBuffer
    val uu = u.filter(p => {
      var res: Boolean = true
      breakable {
        for (i <- 0 until b.size)
          if (eq(b(i), p)) {
            b.remove(i)
            res = false
            break
          }
      }
      res
    }
    )
    if (!b.isEmpty)
      throw new FatalError(Mess(M.CANNOTREMOVEUNITS, (u, toremove)))
    uu
  }

  def getRandom[T](u: Seq[T]): T = getRandom(u, 1)._1.head

  def removeElem[T](a: Seq[T], p: T, eq: (T, T) => Boolean): Seq[T] = removeFromSeq(a, Seq(p), eq)

  private def reuseKilledUnits(b: GameBoard, unitt: CombatUnitType.T) = {
    // add killed units to units
    b.market.units = b.market.units ++ b.market.killedunits.filter(_.utype == unitt)
    // remove units from killed units list
    val newkilledunits: Seq[CombatUnit] = b.market.killedunits.filter(_.utype != unitt)
    b.market.killedunits = newkilledunits
  }

  def checkKilledUnits(b: GameBoard, unitt: CombatUnitType.T): Unit = {
    var units: Seq[CombatUnit] = b.market.units.filter(_.utype == unitt)
    if (units.isEmpty) reuseKilledUnits(b, unitt)
  }

  def getRandomUnit(b: GameBoard, unitt: CombatUnitType.T, remove: Boolean): CombatUnit = {
    checkKilledUnits(b, unitt)
    // again
    val units: Seq[CombatUnit] = b.market.units.filter(_.utype == unitt)
    if (units.isEmpty)
      throw new FatalError(Mess(M.ALLUNITSUSED, (unitt)))
    val u = getRandom(units, 1)
    if (remove)
      b.market.units = b.market.units.filter(_.utype != unitt) ++ u._2
    u._1.head
  }

  def getThreeRandomUnits(b: GameBoard, remove: Boolean): Seq[CombatUnit] = Seq(getRandomUnit(b, CombatUnitType.Infantry, remove), getRandomUnit(b, CombatUnitType.Artillery, remove), getRandomUnit(b, CombatUnitType.Mounted, remove))

  case class CurrentPhase(val notcompleted: Seq[Civilization.T], val turnPhase: TurnPhase.T, val roundno: Int)

  private def getPhase(c: Command): Option[TurnPhase.T] = if (c.command == Command.ENDOFPHASE) Some(c.param.asInstanceOf[TurnPhase.T]) else None

  def allCivs(b: GameBoard): Seq[Civilization.T] = b.players.map(_.civ)

  private def nextPhase(pha: TurnPhase.T): TurnPhase.T = if (pha == TurnPhase.Research) TurnPhase.StartOfTurn else TurnPhase.apply(pha.id + 1)

  private def prevPhase(pha: TurnPhase.T): TurnPhase.T = if (pha == TurnPhase.StartOfTurn) TurnPhase.Research else TurnPhase.apply(pha.id - 1)

  private def currentTurnReverse(b: GameBoard): Seq[Command] = {
    var list: Seq[Command] = Nil
    breakable(
      b.play.commands.reverse.foreach(c => {
        if (getPhase(c) == TurnPhase.StartOfTurn) break
        list = list :+ c
      })
    )
    list
  }

  def currentPhase(b: GameBoard): CurrentPhase = {

    // reverse command, analyze from end
    val rlist: Seq[action.Command] = b.play.commands.reverse
    // collect civ for last phase
    // filter only ENDOFPHASE
    val ephases: Seq[Command] = rlist.filter(getPhase(_).isDefined)

    // beginning of game
    if (ephases.isEmpty) {
      // initiate the order of civ
      b.pllist = allCivs(b)
      return CurrentPhase(b.pllist, TurnPhase.StartOfTurn, 0)
    }

    var players: Set[Civilization.T] = b.pllist.toSet
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
    if (!players.isEmpty) return CurrentPhase(b.pllist.filter(c => players(c)), currentphase, roundno)
    // empty, next phase
    CurrentPhase(b.pllist, nextPhase(currentphase), if (currentphase == TurnPhase.Research) roundno + 1 else roundno)
  }

  private def lastPhaseCommandsReverse(b: GameBoard, civ: Civilization.T, pha: TurnPhase.T): Seq[Command] = {
    val rlist: Seq[Command] = b.play.commands.reverse
    var clist: Seq[Command] = Nil
    val prevP: TurnPhase.T = prevPhase(pha)
    breakable {
      // 2017/10/22 - filter out not belonging to civilization
      rlist.filter(co => co.civ == civ).foreach(c => {
        val p: Option[TurnPhase.T] = getPhase(c)
        if (p.isDefined) {
          // break at the beginning of trade
          if (p.get == TurnPhase.Research) break
          // break at the beginning of phase
          if (p.get == prevP) break
          clist = Nil
        }
        else if (c.civ == civ) clist = clist :+ c
      })
    }
    clist
  }

  def CityAvailableForAction(b: GameBoard, civ: Civilization.T): Seq[P] = {
    // selects cities where city action is already executed
    val p: Seq[Command] = lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement).filter(co => Command.cityActionUnique(co.command))
    // all cities
    var cities: Set[P] = citiesForCivilization(b, civ).map(_.p).toSet
    // from the list of all cities remove cities where city action is already done
    p.foreach(c => cities = cities - c.p)
    // cities availables for action
    cities.toSeq
  }

  def CitiesCanAfford(b: GameBoard, civ: Civilization.T, cost: Int): Seq[P] =
    CityAvailableForAction(b, civ).filter(city => getProductionForCity(b, civ, city).prod >= cost)

  case class Move(val command: Command.T, val p: Option[P])

  case class PlayerMove(val f: PlayerFigures, val moves: Seq[Move]) {
    def begstop: (Move, Move) = (moves.head, moves.reverse.find(_.p.isDefined).get)

    def len: Int = moves.filter(_.p.isDefined).length - 1

    def isFinished = moves.last.command == Command.ENDOFMOVE || moves.last.command == Command.EXPLOREHUT

    def lastp: P = begstop._2.p.get
  }

  private def toFig(co: Command): PlayerFigures = {
    val f: Figures = co.param.asInstanceOf[Figures]
    PlayerFigures(co.civ, f.numberofArmies, f.numberofScouts)
  }

  def civLastMoves(b: GameBoard, civ: Civilization.T): Seq[PlayerMove] = {
    val cu: CurrentPhase = currentPhase(b)
    if (cu.turnPhase != TurnPhase.Movement) return Nil
    if (cu.notcompleted.head != civ) return Nil
    // proper order again
    val p: Seq[Command] = lastPhaseCommandsReverse(b, civ, TurnPhase.Movement).reverse
    // start collecting moves
    var li: Seq[PlayerMove] = Nil
    var fig: PlayerFigures = null
    // if null then Reveal
    var moves: Seq[Move] = Nil
    p.foreach(co => co.command match {
      case Command.MOVE | Command.ENDOFMOVE | Command.EXPLOREHUT =>
        moves = moves :+ Move(co.command, if (co.p == null) None else Some(co.p))
        // 2017/12/23
        if (co.param != null) {
          // 2017/12/23
          // if there was a battle, number of figures to move can be lower then number of figures starting the move
          // some figures could be killed in the battle
          fig = toFig(co)
        }
      case Command.ATTACK | Command.STARTBATTLE | Command.PLAYUNIT | Command.PLAYUNITIRON | Command.ENDBATTLE =>
        moves = moves :+ Move(co.command, None)
      case Command.REVEALTILE => moves = moves :+ moves.last // for reveal repeat last
      case _ => {
        if (fig != null) li = li :+ PlayerMove(fig, moves)
        fig = null
        moves = Nil
        if (co.command == Command.STARTMOVE) {
          fig = toFig(co)
          moves = moves :+ Move(co.command, if (co.p == null) None else Some(co.p))
        }
      }
    })
    if (fig != null) li = li :+ PlayerMove(fig, moves)
    li
  }

  def finishedAtPoint(b : GameBoard, civ: Civilization.T) : Map[P, Figures] = {
    val lastp: Seq[(Figures, P, P)] = civLastMoves(b, civ).map(o => (o.f.toFigures, o.begstop._1.p.get, o.begstop._2.p.get))
    //TODO: can be done better,2 traversals, use mutable map and fold
    //    val startmap : Map[P,Figures] = lastp.map(t => t._3 -> Figures(0,0)) toMap
    //    val lastm: Map[P, Figures] = lastp.map(t => t._3 -> t._1).toMap
    // current points on board belonging to civilization
    //    val current: Seq[(Figures, P)] = allSquares(b).filter(p => p.s.figures.civOccupying.isDefined && p.s.figures.civOccupying.get == civ).map(m => (m.s.figures.toFigures, m.p))
    // sum all figures finishing at given point
    val lastm: Map[P, Figures] = lastp.groupBy(_._3).map(e => e._1 -> e._2.foldLeft[Figures](Figures(0, 0))((f, p) => {
      f + p._1;
      f
    }))
    lastm
  }


  def battleParticipants(b: GameBoard): (P, P) = {
    val cu: CurrentPhase = currentPhase(b)
    val p: Seq[Command] = lastPhaseCommandsReverse(b, cu.notcompleted.head, TurnPhase.Movement)
    val command = p.find(p => p.command == Command.ATTACK).get
    // civ attacker defender
    (civLastMoves(b, cu.notcompleted.head).last.lastp, command.p)
  }


  def isCityAvailableForAction(b: GameBoard, civ: Civilization.T, p: P): Boolean = {
    val cities: Seq[P] = CityAvailableForAction(b, civ)
    cities.find(_ == p).isDefined
  }

  def getCurrentMove(b: GameBoard, civ: Civilization.T): Option[PlayerMove] = {
    val l: Seq[PlayerMove] = civLastMoves(b, civ)
    if (l.isEmpty) None else Some(l.last)
  }

  def isResearchDone(b: GameBoard, civ: Civilization.T): Boolean = !lastPhaseCommandsReverse(b, civ, TurnPhase.Research).isEmpty

  private def commandForPhase(b: GameBoard, command: Command): Mess = {
    val current: CurrentPhase = currentPhase(b)
    val phase: TurnPhase.T = Command.actionPhase(command.command)
    if (phase != null && phase != current.turnPhase) return Mess(M.ACTIONCANNOTBEEXECUTEDINTHISPHASE, (command, current.turnPhase, phase))
    if (phase != null && phase == TurnPhase.CityManagement) {
      // check if city correct
      checkP(b, command.p)
      var res: Option[Mess] = checkCity(b, command.p)
      if (res.isDefined) return res.get
      val ss: MapSquareP = getSquare(b, command.p)
      if (!ss.s.cityhere || ss.s.city.get.civ != command.civ) return (Mess(M.THEREISNOCIVLIZATIONCITYATTHISPOINT, (command)))
      // duplicate CityManagement for the same city
      //      val cities: Seq[P] = CityAvailableForAction(b, command.civ)
      //      if (cities.find(_ == command.p).isEmpty) return Mess(M.DUPLICATECITYACTIONINTHISCITY, (command))
      if (Command.cityActionUnique(command.command) && !isCityAvailableForAction(b, command.civ, command.p)) return Mess(M.DUPLICATECITYACTIONINTHISCITY, (command))
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
      //      val p: Seq[Command] = lastPhaseCommandsReverse(b, command.civ, TurnPhase.Research)
      // 2018/01/05
      if (isResearchDone(b, command.civ)) return Mess(M.CANNOTRESEARCHMORETHENONCEINSINGLETURN, command)
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

  // ====================================
  // TRADE
  // ====================================

  private def filterspendCommands(b: GameBoard, civ: Civilization.T, pfilt: (Command) => Boolean): Map[P, Seq[Command]] = {
    val p: Seq[Command] = lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement).reverse
    p.filter(pfilt).groupBy(_.p)
  }


  private def spendProdForCity(c: Seq[Command]): Int = {
    val no = c.foldLeft(0) { (sum, i) =>
      if (i.command == Command.UNDOSPENDTRADE) 0 else {
        val prod: Int = i.param.asInstanceOf[Int]
        sum + prod
      }
    }
    no
  }


  private def spendTradeCommands(b: GameBoard, civ: Civilization.T): Map[P, Seq[Command]] = filterspendCommands(b, civ, c => c.command == Command.UNDOSPENDTRADE || c.command == Command.SPENDTRADE)

  def spendProdForCities(b: GameBoard, civ: Civilization.T): Map[P, Int] =
    spendTradeCommands(b, civ) map { case (p, seq) => (p, spendProdForCity(seq)) }

  case class TradeForCiv(val terrain: Int, val noresearch: Int, val toprod: Int, val loottrade: Int) {
    def trade: Int = Math.min(terrain + noresearch - toprod + loottrade, TRADEMAX)
  }


  private def numberofloottrade(b: GameBoard, civ: Civilization.T): Int = {
    val loott: Int = currentTurnReverse(b).foldLeft(0)((sum, c) => {
      var modif: Int = 0
      if (c.command == Command.TAKEWINNERLOOT) {
        val take: TakeWinnerLoot = c.param.asInstanceOf[TakeWinnerLoot]
        if (take.loot.trade)
          if (take.winner == civ) modif = take.trade
          else if (take.loser == civ) modif = 0 - take.trade
      }
      sum + modif
    }
    )
    loott
  }

  private def numberofTradeTerrain(b: GameBoard, civ: Civilization.T): Integer = {
    val num: Int = citiesForCivilization(b, civ).flatMap(p => squaresAround(b, p.p)).map(_.numberOfTrade).foldLeft(0)(_ + _)
    if (gameStart(b)) num * 2 else num
  }

  private def reduceTradeBySpend(b: GameBoard, civ: Civilization.T, playerLimits: PlayerLimits): Int = {
    playerLimits.prodForTrade(spendProdForCities(b, civ).foldLeft(0) { (sum, i) => sum + i._2 })
  }

  private def numberofTradenoresearch(b: GameBoard, civ: Civilization.T): Integer = {
    // commands : reverse
    val rlist: Seq[Command] = b.play.commands.reverse.filter(_.civ == civ)

    var lasttrade: Int = 0
    var wasresearch: Boolean = false
    breakable {
      rlist.foreach(c => {
        val pha: Option[TurnPhase.T] = getPhase(c)
        if (pha.isDefined)
          if (pha.get == TurnPhase.Research) {
            lasttrade = c.param1.asInstanceOf[Int]
            wasresearch = true
          }
          else if (wasresearch) break

        if (c.command == Command.RESEARCH) {
          lasttrade = 0
          break
        }
      }
      )
    }
    lasttrade
  }

  def numberofTrade(b: GameBoard, civ: Civilization.T): TradeForCiv = {
    val li: PlayerLimits = getLimits(b, civ)
    TradeForCiv(numberofTradeTerrain(b, civ), numberofTradenoresearch(b, civ), reduceTradeBySpend(b, civ, li), numberofloottrade(b, civ))
  }

  // ===================================
  // production for city
  // ===================================

  // scout => city
  def sendprodForScouts(b: GameBoard, civ: Civilization.T): Map[P, P] = {
    val p: Seq[Command] = lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement).reverse.filter(co => co.command == Command.SENDPRODUCTION || co.command == Command.UNDOSENDPRODUCTION)
    val ma: Map[P, Seq[Command]] = p.groupBy(_.param.asInstanceOf[P]).filter(_._2.last.command == Command.SENDPRODUCTION)
    ma.map(pp => pp._1 -> pp._2.last.p)
  }

  // list of scouts used already for harvesting
  private def scoutForHarvest(b: GameBoard, civ: Civilization.T): Set[P] =
    lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement).filter(_.civ == Command.HARVESTRESOURCE).
      map(_.param.asInstanceOf[P]) toSet

  // all scouts used for harvesting and sending production
  private def scoutsUsedAlready(b: GameBoard, civ: Civilization.T): Set[P] = {
    val harv: Set[P] = scoutForHarvest(b, civ)
    val sends: Set[P] = sendprodForScouts(b, civ).map(_._1) toSet

    harv.union(sends)
  }

  private def allScouts(b: gameboard.GameBoard, civ: Civilization.T): Seq[MapSquareP] = getFigures(b, civ).filter(_.s.figures.numberofScouts > 0)

  private def allOutSkirts(b: gameboard.GameBoard, civ: Civilization.T): Seq[MapSquareP] =
    citiesForCivilization(b, civ).map(p => squaresAround(b, p.p)).flatten

  def scoutsAvailableForAction(b: GameBoard, civ: Civilization.T, pfilt: (MapSquareP) => Boolean): Seq[(P, P)] = {
    val out: Seq[MapSquareP] = allOutSkirts(b, civ)
    // filter out scout on outskirts
    // filter also all on squares without production
    val fig: Seq[MapSquareP] = allScouts(b, civ).filter(sc => out.find(_.p == sc.p).isEmpty).filter(pfilt)
    val c: Seq[P] = CityAvailableForAction(b, civ)
    //    val scoutsused : Map[P,P] = sendprodForScouts(b,civ)
    val scoutsused: Set[P] = scoutsUsedAlready(b, civ)
    // filter out all scouts used already
    val scouts: Seq[P] = fig.filter(sc => !scoutsused.contains(sc.p)).map(_.p)
    // all cities x all scouts, cartesian product
    c.map(cit => scouts.map(sc => (cit, sc))).flatten
  }

  def verifyScoutForAction(b: GameBoard, civ: Civilization.T, param: P): Option[Mess] = {
    val fig: Seq[MapSquareP] = allScouts(b, civ)
    val scout: P = param
    if (fig.find(_.p == param).isEmpty) return Some(Mess(M.NOSCOUTATTHISPOINT, (scout)))
    val out: Seq[MapSquareP] = allOutSkirts(b, civ)
    if (out.find(_.p == scout).isDefined)
      return return Some(Mess(M.SCOUTISONCITYOUTSKIRT, (scout)))
    return None

  }

  // City => sendProd
  private def sendprodForCities(b: GameBoard, civ: Civilization.T): Map[P, Int] = {
    val ma: Map[P, P] = sendprodForScouts(b, civ)
    ma.foldLeft(Map.empty[P, Int])({
      (ma, p) =>
        val c: Option[Int] = ma.get(p._2)
        val prod: Int = getSquare(b, p._1).numberOfProduction
        if (c.isEmpty) ma + (p._2 -> prod)
        else ma + (p._2 -> (prod + c.get))
    })
  }

  case class ProdForCity(val terrain: Int, val fromtrade: Int, val fromscouts: Int) {
    def prod: Int = terrain + fromtrade + fromscouts
  }

  def getProductionForCity(b: GameBoard, civ: Civilization.T, p: P): ProdForCity = {
    val num: Int = squaresAround(b, p).map(s => getSquare(b, s.p).numberOfProduction).foldLeft(0)(_ + _)
    val prod: Option[Int] = spendProdForCities(b, civ).get(p)
    val prodfromtrade: Int = if (prod.isDefined) prod.get else 0
    val prodFromScouts: Map[P, Int] = sendprodForCities(b, civ)
    val prodfromS: Option[Int] = prodFromScouts.get(p)
    val prodfromscouts: Int = if (prodfromS.isEmpty) 0 else prodfromS.get
    ProdForCity(num, prodfromtrade, prodfromscouts)
  }

  // ==============================================

  case class PlayerLimits(val citieslimit: Int, val stackinglimit: Integer, val watercrossingallowed: Boolean, val waterstopallowed: Boolean,
                          val armieslimit: Int, val scoutslimit: Int, val travelSpeed: Int, val tradeforProd: Int,
                          val playerStrength: CombatUnitStrength, val aircraftUnlocked: Boolean, val scoutscanExplore: Boolean, val isFundametialism: Boolean, val combatBonus: Int) {

    def prodForTrade(prod: Int): Int = prod * tradeforProd
  }

  def getLimits(b: GameBoard, civ: Civilization.T): PlayerLimits = {
    val deck: PlayerDeck = b.playerDeck(civ)
    val citieslimit: Int = deck.defaultcitylimit - citiesForCivilization(b, civ).length
    val count: (Int, Int) = getNumberOfArmies(b, civ)
    val armieslimit: Int = deck.defaultarmieslimit - count._1
    val scoutslimit: Int = deck.defaultscoutslimit - count._2
    PlayerLimits(citieslimit, deck.defaultstackinglimit, false, false, armieslimit, scoutslimit, deck.defaulttravelspeed, DEFAULTTRADEFORPROD, deck.combatlevel, false, false, false, 0)
  }

  // =====================================
  // Figures
  // =====================================


  def getFigures(b: GameBoard, civ: Civilization.T): Seq[MapSquareP] = allSquares(b).filter(s => s.s.figures.civOccupying(civ))

  def getNumberOfArmies(b: GameBoard, civ: Civilization.T): (Int, Int) =
    getFigures(b, civ).foldLeft((0, 0))((s1, s2) => (s1._1 + s2.s.figures.numberofArmies, s1._2 + s2.s.figures.numberofScouts))

  def isSquareForFigures(b: GameBoard, civ: Civilization.T, f: Figures, s: MapSquare, li: PlayerLimits): Option[Mess] = {
    if (!s.figures.empty) {
      if (!s.figures.civOccupying(civ)) return Some(Mess(M.CANNOTSETFIGUREONALIENCIV, s))
      if (s.figures.numberofArmies + s.figures.numberofScouts + f.numberofScouts + f.numberofArmies > li.stackinglimit) return Some(Mess(M.STACKINGSIZEEXCEEDED, s))
    }
    if (s.cityhere)
      if (!s.city.get.belongsTo(civ)) return Some(Mess(M.CANNOTSETFGUREONALIENCITY, s))
    if (s.hvhere)
      if (s.hv.get.hv == HutVillage.Hut) return Some(Mess(M.CANNOTSETFIGUREONHUT)) else return Some(Mess(M.CANNOTSETFIGUREONVILLAGE))
    None
  }

  def isSquareForFigure(b: GameBoard, civ: Civilization.T, f: Figure.T, p: P): Option[Mess] = {
    val li: PlayerLimits = getLimits(b, civ)
    val count: (Int, Int) = getNumberOfArmies(b, civ)
    f match {
      case Figure.Army => if (li.armieslimit < 1) return Some(Mess(M.LIMITFORARMIESEXCEEDED, (count._1, li.armieslimit)))
      case Figure.Scout => if (li.scoutslimit < 1) return Some(Mess(M.LIMITFORSCOUTSEXCEEDED, (count._2, li.scoutslimit)))
    }
    val s: MapSquareP = getSquare(b, p)
    if (s.s.cityhere) return Some(Mess(M.CANNOTSETFIGUREONCITY, p))
    if (s.sm.terrain == Terrain.Water && !li.waterstopallowed) return Some(Mess(M.CANNOTPUTFIGUREONWATER, p))
    val fig: Figures = if (f == Figure.Scout) Figures(0, 1) else Figures(1, 0)
    isSquareForFigures(b, civ, fig, s.s, li)
  }

  def canBuyFigure(b: GameBoard, civ: Civilization.T, p: P, f: Figure.T): Option[Mess] = {
    val cost: Int = ObjectCost.getCost(f)
    val prod: Int = getProductionForCity(b, civ, p).prod
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

  def exploreHutOrVillage(b: GameBoard, civ: Civilization.T, p: P) = {
    val m: MapSquareP = getSquare(b, p)
    val h: HutVillage = m.s.hv.get
    m.s.hv = None
    val pl: PlayerDeck = b.playerDeck(civ)
    pl.hvlist = pl.hvlist :+ h
    // final, move figures to point
    moveFigures(b, civ, p, None)
  }

  def moveFigures(b: GameBoard, civ: Civilization.T, p: P, fparam: Option[Figures]) = {
    val fig: PlayerMove = getCurrentMove(b, civ).get
    val last: P = fig.lastp
    val f: Figures = if (fparam.isEmpty) fig.f.toFigures else fparam.get
    // remove from last
    putFigures(b, civ, last, -f)
    // new position
    putFigures(b, civ, p, f)
  }

}
