package civilization

import civilization.action.{Command, constructCommand}
import civilization.gameboard.{BattleFieldSide, _}
import civilization.io.readdir.GameResources
import civilization.message.{FatalError, J, M, Mess}
import civilization.objects._
import play.api.libs.json.{JsNumber, Json}

import scala.util.control.Breaks._

package object helper {

  private val ra = scala.util.Random

  case class MapSquareP(val s: MapSquare, val sm: Square, val p: P, val t: MapTile, val suggestedCapital: Boolean) {
    def revealed: Boolean = t.orientation.isDefined

    //    def terrain: Terrain.T = sm.terrain
    def terrain: Terrain.T = sm.terrain

    def numberOfTrade: Int =
    // if building trade from building
      if (s.building.isDefined) s.building.get.tokens.numofTrade
      else if (s.wonder.isDefined) 0
      else if (s.greatperson.isDefined) s.greatpersonype.tokens.numofTrade
      else sm.token.numofTrade

    def numberOfProduction: Int =
    // if wonder, no production
      if (s.wonder.isDefined) 0
      else if (s.building.isDefined) s.building.get.tokens.numofProduction
      else if (s.greatperson.isDefined) s.greatpersonype.tokens.numofProduction
      else sm.token.numofProduction

    def numofCoins: Int =
      if (s.greatperson.isDefined) s.greatpersonype.tokens.numofCoins
      else if (s.building.isDefined) s.building.get.tokens.numofCoins
      else if (resource.isDefined)
        if (resource.get == Resource.Coin) 1 else 0
      else 0

    def isempty: Boolean =
      if (s.building.isDefined) false
      else if (s.wonder.isDefined) false
      else if (s.greatperson.isDefined) false
      else true

    def resource: Option[Resource.T] =
    // building covers resource
      if (isempty) sm.resource
      else None

    //      if (s.building.isDefined) None
    //      else if (s.wonder.isDefined) None
    //      else if (s.greatperson.isDefined) None
    //      else sm.resource

    def culture: Int =
      if (s.building.isDefined)
        s.building.get.tokens.numofCulture
      else if (s.greatperson.isDefined) s.greatpersonype.tokens.numofCulture
      else if (s.wonder.isDefined) 0
      else if (resource.isEmpty) 0
      else if (resource.get == Resource.Culture) 1 else 0

    def combatBonus: Int =
      if (s.building.isDefined) s.building.get.tokens.numofBattle
      else if (s.greatperson.isDefined) s.greatpersonype.tokens.numofBattle
      else if (s.wonder.isDefined) WonderFeatures.combatBonus(s.wonder.get.w)
      else 0

    def suggestedCapitalForCiv: Option[Civilization.T] = t.tile.civ

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

  def citiesForCivilization(board: GameBoard, civ: Civilization.T): Seq[MapSquareP] =
    allPoints(board).map(getSquare(board, _)).filter(s => s.s.cityhere && s.s.city.get.belongsTo(civ)).toList

  def foundCapitalForCiv(board: GameBoard, civ: Civilization.T): Option[MapSquareP] =
    citiesForCivilization(board, civ).find(s => City.isCapital(s.s.city.get.citytype))

  def isCapitalBuild(board: GameBoard, civ: Civilization.T): Boolean = foundCapitalForCiv(board, civ).isDefined

  private def outskirtsForCiv(b: GameBoard, civ: Civilization.T): Seq[MapSquareP] =
    citiesForCivilization(b, civ).flatMap(p => squaresAround(b, p.p))

  private def squareNotBlocked(b: GameBoard, civ: Civilization.T, s: MapSquareP): Boolean =
    s.civHere.isEmpty || s.civHere.get == civ

  private def outskirtsForCivNotBlocked(b: GameBoard, civ: Civilization.T): Seq[MapSquareP] =
    outskirtsForCiv(b, civ).filter(squareNotBlocked(b, civ, _))

  def outskirtsForCityNotBlocked(b: GameBoard, civ: Civilization.T, p: P): Seq[MapSquareP] =
    squaresAround(b, p).filter(squareNotBlocked(b, civ, _))

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


  private def currentTurnReverseFromSeq(l: Seq[Command]): Seq[Command] = {
    var list: Seq[Command] = Nil
    breakable(
      l.reverse.foreach(c => {
        if (getPhase(c).isDefined && getPhase(c).get == TurnPhase.Research) break
        //        if (getPhase(c).isDefined && getPhase(c).get == TurnPhase.StartOfTurn) break
        list = list :+ c
      })
    )
    list
  }

  private def currentTurnReverse(b: GameBoard, civ: Civilization.T): Seq[Command] =
    currentTurnReverseFromSeq(b.play.commands.filter(_.civ == civ))


  //  private def currentTurnReverseForCiv(b: GameBoard, civ: Civilization.T): Seq[Command] =
  //    currentTurnReverse(b).filter(_.civ == civ)


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

  def commandUsedAlready(b: GameBoard, civ: Civilization.T, pha: TurnPhase.T, com: Command.T): Boolean =
    lastPhaseCommandsReverse(b, civ, pha).exists(_.command == com)

  def lastCommand(b: GameBoard, civ: Civilization.T, com: Command.T): Boolean = {
    val last: Seq[Command] = currentTurnReverse(b, civ)
    !last.isEmpty && last.head.command == com
  }

  //  def technologyResourceUsed(b: GameBoard, civ: Civilization.T): Boolean = {
  // all commands in the current phase
  //    val com: Seq[Command] = currentTurnReverse(b, civ)
  // only technology commands
  // technology resource command is used
  //    com.find(co => Command.isTechnologyResourceAction(co.command)).isDefined
  //  }

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

  def CitiesCanAfford(b: GameBoard, civ: PlayerDeck, cost: Int): Seq[P] =
    CityAvailableForAction(b, civ).filter(city => getProductionForCity(b, civ, city).prod >= cost)

  def CitiesCanAfford(b: GameBoard, civ: PlayerDeck, com: Command.T): Seq[P] =
    CitiesCanAfford(b, civ, CityActionCost.actionCost(com))

  case class Move(val command: Command.T, val p: Option[P])

  case class PlayerMove(val f: PlayerFigures, val moves: Seq[Move]) {
    def begstop: (Move, Move) = (moves.head, moves.reverse.find(_.p.isDefined).get)

    def len: Int = moves.filter(_.p.isDefined).length - 1

    def isFinished = moves.last.command == Command.ENDOFMOVE || moves.last.command == Command.EXPLOREHUT || moves.last.command == Command.KILLFIGURE

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
      case Command.MOVE | Command.ENDOFMOVE | Command.EXPLOREHUT | Command.KILLFIGURE =>
        moves = moves :+ Move(co.command, if (co.p == null) None else Some(co.p))
        // 2017/12/23
        if (co.param != null) {
          // 2017/12/23
          // if there was a battle, number of figures to move can be lower then number of figures starting the move
          // some figures could be killed in the battle
          fig = toFig(co)
        }
      case Command.ATTACK | Command.STARTBATTLE | Command.PLAYUNIT | Command.PLAYUNITIRON | Command.ENDBATTLE | Command.SAVEUNIT =>
        moves = moves :+ Move(co.command, None)
      case Command.REVEALTILE => moves = moves :+ moves.last // for reveal repeat last
      case Command.FREENONUPGRADEDBUILDING => // ignore
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

  def finishedAtPoint(b: GameBoard, civ: Civilization.T): Map[P, Figures] = {
    val lastp: Seq[(Figures, P, P)] = civLastMoves(b, civ).map(o => (o.f.toFigures, o.begstop._1.p.get, o.begstop._2.p.get))
    //TODO: can be done better,2 traversals, use mutable map and fold
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

  def isResearchDone(b: GameBoard, deck: PlayerDeck): Boolean = !lastPhaseCommandsReverse(b, deck.civ, TurnPhase.Research).isEmpty

  private def commandForPhase(b: GameBoard, command: Command): Mess = {
    // technology resource action only once
    //    if (Command.isTechnologyResourceAction(command.command))
    //      if (technologyResourceUsed(b, command.civ)) return Mess(M.RESOURCEAVAILIBILITYCANBEUSERONCEPERTURN, command)
    val current: CurrentPhase = currentPhase(b)
    val phase: TurnPhase.T = Command.actionPhase(command.command)
    if (phase != null && phase != current.turnPhase) return Mess(M.ACTIONCANNOTBEEXECUTEDINTHISPHASE, (command, current.turnPhase, phase))
    if (phase != null && phase == TurnPhase.CityManagement && Command.cityActionUnique(command.command)) {
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
      if (isResearchDone(b, b.playerDeck(command.civ)) && command.command == Command.RESEARCH) return Mess(M.CANNOTRESEARCHMORETHENONCEINSINGLETURN, command)
    }
    null
  }

  def playsingleCommand(b: GameBoard, command: Command, f: Command => Unit = p => Unit): Mess = {
    var m: Mess = commandForPhase(b, command)
    if (m != null) return m
    // test if point on board
    if (command.p != null && !Command.commandNotPoint(command.command) && !isPointOnBoard(b, command.p)) return Mess(M.POINTOUTSIDEBOARD, command.p)
    m = command.verifyCommand(b)
    if (m != null) {
      return m
    }
    command.executeCommand(b)
    f(command)
    b.play.addCommand(command)
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

  def firstRound(b: GameBoard, phase: Option[TurnPhase.T]): Boolean = {
    val c = currentPhase(b)
    if (c.roundno != 0) return false
    if (phase.isEmpty) return true
    return phase.get == c.turnPhase
  }

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

  case class TradeForCivCalculate(val terrain: Int, val noresearch: Int, val toprod: Int, val tradecoins: Int, val tradespendCulture: Int, val tradefromGreatLight: Int) {
    def trade: Int = math.min(terrain + noresearch + tradefromGreatLight - toprod + tradecoins - tradespendCulture, TRADEMAX)
  }

  case class TradeForCiv(val tradecalculated: Int, val toprod: Int, val spendOnCulture: Int, val increased: Int, val decreased : Int) {
    require(increased >= 0 && decreased >= 0)
//    def trade: Int = math.min(tradecalculated - toprod - spendOnCulture + increased, TRADEMAX)
    def trade : Int = math.min(tradecalculated + increased, TRADEMAX) - toprod - spendOnCulture - decreased
  }


  private def numberofloottrade(b: GameBoard, civ: Civilization.T): Int = {
    val loott: Int = currentTurnReverseFromSeq(b.play.commands).foldLeft(0)((sum, c) => {
      var modif: Int = 0
      sum + modif
    }
    )
    loott
  }

  private def numberofTradeTerrain(b: GameBoard, civ: Civilization.T): Int = {
    //    val num: Int = citiesForCivilization(b, civ).flatMap(p => squaresAround(b, p.p)).map(_.numberOfTrade).foldLeft(0)(_ + _)
    val num: Int = outskirtsForCivNotBlocked(b, civ).map(_.numberOfTrade).sum
    // squareNotBlocked(b, civ, s
    if (firstRound(b, None)) num * 2 else num
  }

  private def reduceTradeBySpend(b: GameBoard, civ: Civilization.T, playerLimits: PlayerLimits): Int = {
    playerLimits.prodForTrade(spendProdForCities(b, civ).foldLeft(0) { (sum, i) => sum + i._2 })
  }


  private def numberofTradePhasenoresearch(b: GameBoard, civ: Civilization.T, phase: TurnPhase.T): Int = {
    // commands : reverse
    val rlist: Seq[Command] = b.play.commands.reverse.filter(_.civ == civ)

    var lasttrade: Int = 0
    var wasresearch: Boolean = false
    breakable {
      rlist.foreach(c => {
        val pha: Option[TurnPhase.T] = getPhase(c)
        if (pha.isDefined)
          if (pha.get == phase) {
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

  private def tradeSpendOnCulture(b: GameBoard, civ: Civilization.T): Int = {
    val com: Seq[Command] = currentTurnReverse(b, civ)
    com.foldLeft(0) { (sum, i) => {
      if (i.command == Command.ADVANCECULTURE) {
        val cul: CultureTrack.CultureTrackCost = i.param1.asInstanceOf[CultureTrack.CultureTrackCost]
        sum + cul.trade
      }
      else sum
    }
    }
  }


  private def numberofTradenoresearch(b: GameBoard, civ: Civilization.T): Int =
    numberofTradePhasenoresearch(b, civ, TurnPhase.Research)

  private def numberofTradeTradenoresearch(b: GameBoard, civ: Civilization.T): Int =
    numberofTradePhasenoresearch(b, civ, TurnPhase.Trade)

  private def numberofFromCommand(b: GameBoard, deck: PlayerDeck, com: Command.T): Option[(Command, Int)] = {
    val coma: Option[Command] = currentTurnReverse(b, deck).find(_.command == com)
    if (coma.isEmpty) None
    else Some((coma.get, coma.get.param.asInstanceOf[Int]))
  }

  private def numberofTradeFromIncrease(b: GameBoard, deck: PlayerDeck, com: Command.T): (Int,Int) = {
    val listofi : Seq[Int] = currentTurnReverse(b, deck).filter(_.command == com).map(co => co.param.asInstanceOf[Int])
    val sumplus: Int = listofi.filter(_ > 0).sum
    val summinus : Int = ( 0 - listofi.filter(_ < 0).sum)
    (sumplus,summinus)
  }

  def numberofTrade(b: GameBoard, deck: PlayerDeck): TradeForCiv = {
    if (b.tradecurrent) {
      // cheating
      val tra = numberofTradeCalculate(b, deck)
      return TradeForCiv(tra.trade, 0, 0, 0,0)
    }
    val li: PlayerLimits = getLimits(b, deck)
    val tradecalculated: Int = numberofTradeTradenoresearch(b, deck.civ)
    val spendonprod: Int = reduceTradeBySpend(b, deck.civ, li)
    val spendonculture: Int = tradeSpendOnCulture(b, deck.civ)
    val (sumplus: Int, summinus : Int) = numberofTradeFromIncrease(b, deck, Command.INCREASETRADE)

    TradeForCiv(tradecalculated, spendonprod, spendonculture, sumplus,summinus)
  }

  private def tradefromGreatLight(b: GameBoard, deck: PlayerDeck): Int =
    if (!hasWonderFeature(b, deck, WonderFeatures.extratradeFromEmptyWater)) 0
    else
      outskirtsForCivNotBlocked(b, deck).filter(s => s.isempty && s.terrain == Terrain.Water) length

  def numberofTradeCalculate(b: GameBoard, deck: PlayerDeck): TradeForCivCalculate = {
    val li: PlayerLimits = getLimits(b, deck)
    TradeForCivCalculate(numberofTradeTerrain(b, deck), numberofTradenoresearch(b, deck), reduceTradeBySpend(b, deck, li),
      getCoins(b, deck).coins, tradeSpendOnCulture(b, deck), tradefromGreatLight(b, deck))
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
    lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement).filter(_.command == Command.HARVESTRESOURCE).
      map(_.param.asInstanceOf[P]) toSet

  private def scoutsForCulture(b: GameBoard, civ: Civilization.T): Seq[P] =
    lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement).filter(_.command == Command.DEVOUTTOCULTURE).
      map(_.param.asInstanceOf[Seq[P]]) flatten

  // all scouts used for harvesting and sending production and culture
  private def scoutsUsedAlready(b: GameBoard, civ: Civilization.T): Set[P] = {
    val harv: Set[P] = scoutForHarvest(b, civ)
    val sends: Set[P] = sendprodForScouts(b, civ).map(_._1) toSet
    val culture: Set[P] = scoutsForCulture(b, civ) toSet

    harv.union(sends).union(culture)
  }

  private def allScouts(b: gameboard.GameBoard, civ: Civilization.T): Seq[MapSquareP]
  = getFigures(b, civ).filter(_.s.figures.numberofScouts > 0)


  private def allScoutsOutside(b: gameboard.GameBoard, civ: Civilization.T): Seq[MapSquareP] = {
    val out: Seq[MapSquareP] = outskirtsForCiv(b, civ)
    val sq: Seq[MapSquareP] = allScouts(b, civ)
    sq.filter(sc => out.find(_.p == sc.p).isEmpty)
  }

  def scoutsAvailableForAction(b: GameBoard, civ: Civilization.T, pfilt: (MapSquareP) => Boolean): Seq[(P, P)] = {
    //    val out: Seq[MapSquareP] = outskirtsForCiv(b, civ)
    // filter out scout on outskirts
    // filter also all on squares without production
    //    val fig: Seq[MapSquareP] = allScouts(b, civ).filter(sc => out.find(_.p == sc.p).isEmpty).filter(pfilt)
    val fig: Seq[MapSquareP] = allScoutsOutside(b, civ).filter(pfilt)
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
    val out: Seq[MapSquareP] = outskirtsForCiv(b, civ)
    if (out.find(_.p == scout).isDefined)
      return Some(Mess(M.SCOUTISONCITYOUTSKIRT, (scout)))
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


  case class ProdForCity(val terrain: Int, val fromtrade: Int, val fromscouts: Int, val fromwheat: Int, val fromwonders: Int, val prodFromCoins: Int) {
    def prod: Int = terrain + fromtrade + fromscouts + fromwheat + fromwonders + prodFromCoins
  }

  private def getProductionForCityWheat(b: GameBoard, deck: PlayerDeck, p: P): Int = {
    val o = numberofFromCommand(b, deck, Command.INCREASEPRODUCTION)
    if (o.isEmpty) 0
    else if (o.get._1.p == p) o.get._2 else 0
  }

  private def incrProdForCoins(b: GameBoard, civ: PlayerDeck): Int =
    if (civ.hasTechnologyFeature(TechnologyFeatures.increaseProdForCoins))
      getCoins(b, civ).coins / 3 // production for every 3 coins
    else 0


  def getProductionForCity(b: GameBoard, civ: PlayerDeck, p: P): ProdForCity = {

    val num: Int = outskirtsForCityNotBlocked(b, civ, p).map(_.numberOfProduction).sum

    val prod: Option[Int] = spendProdForCities(b, civ).get(p)
    val prodfromtrade: Int = if (prod.isDefined) prod.get else 0
    val prodFromScouts: Map[P, Int] = sendprodForCities(b, civ)
    val prodfromS: Option[Int] = prodFromScouts.get(p)
    val prodfromscouts: Int = if (prodfromS.isEmpty) 0 else prodfromS.get
    var fromwonders: Int = if (hasWonderFeature(b, civ, WonderFeatures.increaseProduction3ForCities)) 3 else 0
    ProdForCity(num, prodfromtrade, prodfromscouts, getProductionForCityWheat(b, civ, p), fromwonders, incrProdForCoins(b, civ))
  }

  // ======================================

  case class EconomyForCiv(val tech: Int, val squares: Int, val scout: Int, val techabilities: Int, val coinsdeck: Int) {
    def coins = tech + squares + scout + techabilities + coinsdeck
  }

  def getCoins(b: GameBoard, pl: PlayerDeck): EconomyForCiv = {
    // check technologies
    val tech: Int = pl.tech.map(_.coins).sum
    // coins from tech abilities
    val techabilities: Int = pl.tech.map(t => b.getTech(t.tech)).map(te => if (te.coins.isEmpty) 0 else te.coins.get).sum

    // number of squares with coins
    val squares: Int = outskirtsForCivNotBlocked(b, pl).map(m => m.numofCoins).sum
    val scouts: Int = allScoutsOutside(b, pl).filter(s => s.resource.isDefined && s.resource.get == Resource.Coin).length
    EconomyForCiv(tech, squares, scouts, techabilities, pl.resou.nof(Resource.Coin))
  }

  // ==============================================

  private def calculateCombatBonus(b: GameBoard, civ: Civilization.T): Int =
  // all buildings
    outskirtsForCivNotBlocked(b, civ).map(_.combatBonus).sum

  // ==============================================

  case class PlayerLimits(val citieslimit: Int, val stackinglimit: Int, val watercrossingallowed: Boolean, val waterstopallowed: Boolean,
                          val armieslimit: Int, val scoutslimit: Int, val travelSpeed: Int, val tradeforProd: Int,
                          val playerStrength: CombatUnitStrength, val aircraftUnlocked: Boolean, val scoutscanExplore: Boolean, val combatBonus: Int, val handsize: Int,
                          val prodfortrade: Int) {

    def prodForTrade(prod: Int): Int = (prod / prodfortrade).toInt * tradeforProd
  }

  def getLimits(b: GameBoard, deck: PlayerDeck): PlayerLimits = {
    val citieslimit: Int = (if (deck.hasTechnologyFeature(TechnologyFeatures.citiesLimitIs3)) 3 else DEFAULTCITYLIMIT) - citiesForCivilization(b, deck.civ).length
    val count: (Int, Int) = getNumberOfArmies(b, deck.civ)
    val armieslimit: Int = deck.defaultarmieslimit - count._1
    val scoutslimit: Int = deck.defaultscoutslimit - count._2
    val handsize: Int = deck.defaultculturehandsize + deck.numofTechnologyFeatures(TechnologyFeatures.increaseHandSize)

    val tradeforprod: Int = DEFAULTTRADEFORPROD;
    val prodfortrade: Int = CivilizationFeatures.prodfortrade(deck.civ)
    val technologytravelspeed: Int =
      math.max(DEFAULTTRAVELSPPED, if (deck.tech.isEmpty) 0 else deck.tech.map(t => TechnologyFeatures.speedLimit(t.tech)).max)
    val travelspeed: Int = technologytravelspeed + (if (CivilizationFeatures.increaseTravelSpeedByOne(deck.civ)) 1 else 0)

    PlayerLimits(citieslimit, deck.stackLimit,
      deck.hasTechnologyFeature(TechnologyFeatures.watercrossingAllowed),
      deck.hasTechnologyFeature(TechnologyFeatures.canStopInWater),
      armieslimit, scoutslimit, travelspeed, tradeforprod, deck.combatlevel, false, false, calculateCombatBonus(b, deck.civ), handsize, prodfortrade)
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

  def isSquareForFigure(b: GameBoard, deck: PlayerDeck, f: Figure.T, p: P): Option[Mess] = {
    val li: PlayerLimits = getLimits(b, deck)
    val count: (Int, Int) = getNumberOfArmies(b, deck.civ)
    f match {
      case Figure.Army => if (li.armieslimit < 1) return Some(Mess(M.LIMITFORARMIESEXCEEDED, (count._1, li.armieslimit)))
      case Figure.Scout => if (li.scoutslimit < 1) return Some(Mess(M.LIMITFORSCOUTSEXCEEDED, (count._2, li.scoutslimit)))
    }
    val s: MapSquareP = getSquare(b, p)
    if (s.s.cityhere) return Some(Mess(M.CANNOTSETFIGUREONCITY, p))
    if (s.sm.terrain == Terrain.Water && !li.waterstopallowed) return Some(Mess(M.CANNOTPUTFIGUREONWATER, p))
    val fig: Figures = if (f == Figure.Scout) Figures(0, 1) else Figures(1, 0)
    isSquareForFigures(b, deck.civ, fig, s.s, li)
  }

  def canBuyFigure(b: GameBoard, civ: PlayerDeck, p: P, f: Figure.T): Option[Mess] = {
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

  def checkFinalPoint(b: GameBoard, deck: PlayerDeck, s: MapSquareP, fig: Figures): Option[Mess] = {
    val li: PlayerLimits = getLimits(b, deck)
    val figdesc: (P, Figures) = (s.p, fig)
    // 2017/12/30 : error, water
    if (s.sm.terrain == Terrain.Water && !li.waterstopallowed) return Some(Mess(M.CANNOTSTOPINWATER, figdesc))
    if (s.s.cityhere && s.s.city.get.belongsTo(deck.civ)) return Some(Mess(M.CANNOTSTOPINCITY, figdesc))
    // 2017/08/28 figures already on the point
    None
  }

  private def directMove(p: P, next: P): Boolean = {
    // removed: 2017.08.21
    //    if (p.row == next.row && p.col == next.col) return true
    if ((p.row == next.row) && ((p.col + 1 == next.col) || (p.col - 1 == next.col))) return true
    if ((p.col == next.col) && ((p.row + 1 == next.row) || (p.row - 1 == next.row))) return true
    false
  }

  case class FiguresParam(f: Figures, lastp: P, len: Int)

  def toFiguresParam(fig: PlayerMove): FiguresParam = FiguresParam(fig.f.toFigures, fig.lastp, fig.len)

  def figureMovePointCheck(b: GameBoard, deck: PlayerDeck, fig: FiguresParam, p: P, endofmove: Boolean): Option[Mess] = {
    assert(p != null || endofmove)
    if (p == null) return checkFinalPoint(b, deck, getSquare(b, fig.lastp), fig.f)
    val figdesc: (P, FiguresParam) = (p, fig)
    // endofmove can be the same point as last
    if (!endofmove || fig.lastp != p)
      if (!directMove(fig.lastp, p)) return Some(Mess(M.MOVENOTCONSECUTIVE, figdesc))
    val li: PlayerLimits = getLimits(b, deck)
    // ignore first STARTMOVE
    if (fig.len + 1 > li.travelSpeed) return Some(Mess(M.TRAVELSPEEDEXCEEDED, (figdesc, li.travelSpeed)))
    val s: MapSquareP = getSquare(b, p)
    if (!s.revealed) return Some(Mess(M.POINTNOTREVEALED, p))
    if (s.sm.terrain == Terrain.Water && !li.watercrossingallowed) return Some(Mess(M.CANNOTCROSSWATER, figdesc))
    val mess: Option[Mess] = isSquareForFigures(b, deck.civ, fig.f, s.s, li)
    if (mess.isDefined) return mess
    if (endofmove) checkFinalPoint(b, deck, s, fig.f) else None
  }


  def putFigures(b: GameBoard, civ: Civilization.T, p: P, f: Figures) = {
    val s: MapSquareP = getSquare(b, p)
    s.s.figures.civ = civ
    s.s.figures + f
  }

  def exploreHutOrVillage(b: GameBoard, pl: PlayerDeck, p: P) = {
    val m: MapSquareP = getSquare(b, p)
    val h: HutVillage = m.s.hv.get
    m.s.hv = None
    pl.hvlist = pl.hvlist :+ h
    // final, move figures to point
    moveFigures(b, pl, p, None)
  }

  def moveFigures(b: GameBoard, civ: Civilization.T, p: P, fparam: Option[Figures], kill: Boolean = false) = {
    val fig: PlayerMove = getCurrentMove(b, civ).get
    val last: P = fig.lastp
    val f: Figures = if (fparam.isEmpty) fig.f.toFigures else fparam.get
    // remove from last
    putFigures(b, civ, last, -f)
    // new position
    if (!kill) putFigures(b, civ, p, f)
  }

  def cultureforhutvillage(b: GameBoard, civ: Civilization.T, isExecute: Boolean) =
    if (isExecute && CivilizationFeatures.get3CultureForHutOrVillage(civ))
      b.increaseCultureCommand(civ, 3)


  def allMoves(b: GameBoard, deck: PlayerDeck, fig: Figures, startp: P, foreigncities: Boolean): Seq[P] = {
    // search breadth
    val lim = getLimits(b, deck) // speed
    var visited: Set[P] = Set()
    var foreign: Set[P] = Set()
    var level: Seq[P] = Seq(startp) // current level, starting point
    for (i <- 1 to lim.travelSpeed) {
      var nextlevel: Seq[P] = Nil
      val endofmove: Boolean = i == lim.travelSpeed
      level.foreach(p => {
        if (!(visited contains p)) {
          val around: Seq[MapSquareP] = squaresAround(b, p)
          around.foreach(nextp => {
            val check: Option[Mess] = figureMovePointCheck(b, deck, FiguresParam(fig, p, i - 1), nextp.p, endofmove)
            if (check.isEmpty) nextlevel = nextlevel :+ nextp.p
            else if (check.get.m == M.CANNOTSETFGUREONALIENCITY) foreign = (foreign + nextp.p)
          })
          visited = visited + p
        }
      }
      )
      level = nextlevel
    }
    if (foreigncities) foreign toSeq
    else (visited - startp) toSeq
  }

  // ===================================
  // buildings
  // ===================================

  def removeBuilding(b: GameBoard, s: MapSquareP) = {
    // return building to the market
    b.market.buildings.incdecBuilding(s.s.building.get.name, true)
    // remove building
    s.s.removeBuilding()
  }

  def removeWonder(b: GameBoard, s: MapSquareP) = s.s.wonder = None

  def getStructureHere(p: MapSquareP): Seq[P] = if (p.s.building.isDefined || p.s.wonder.isDefined || p.s.greatperson.isDefined) Seq(p.p) else Nil

  def getOutskirtsForBuild(b: GameBoard, civ: Civilization.T, city: P): Seq[MapSquareP] = outskirtsForCityNotBlocked(b, civ, city).
    filter(_.sm.terrain != Terrain.Water)


  def removeStructure(b: GameBoard, ma: MapSquareP) = {
    if (ma.s.building.isDefined)
      removeBuilding(b, ma)
    // remove wonder if exist
    if (ma.s.wonder.isDefined)
      removeWonder(b, ma)
    // remove great person
    if (ma.s.greatperson.isDefined)
      ma.s.greatperson = None
  }

  // ===========================
  // culture for city
  // ===========================
  case class CultureForCity(val cityculture: Int, val outskirts: Int, val scouts: Int) {
    def culture: Int = cityculture + outskirts + scouts
  }

  def cultureForCity(b: GameBoard, p: P): CultureForCity = {
    val c: Option[Mess] = checkCity(b, p)
    if (c.isDefined) throw FatalError(c.get)
    val ss: MapSquareP = getSquare(b, p)
    val cityculture: Int = if (City.isCapital(ss.s.city.get.citytype)) CULTURECAPITAL else CULTURECITY
    val outskirts: Int = outskirtsForCityNotBlocked(b, ss.civHere.get, p).map(_.culture).sum
    val scouts: Int = 0
    return CultureForCity(cityculture, outskirts, scouts)
  }

  // ==================================
  // journal
  // ==================================

  def addToJournal(b: GameBoard, civ: Civilization.T, j: J.J, content: AnyRef, tech: Option[TechnologyName.T] = None) = {
    val curr = currentPhase(b)
    b.addJ(JournalElem(j, curr.turnPhase, curr.roundno, civ, content, tech))
  }

  // ==================================
  // PlayerTechnology
  // ==================================
  def addCoinToTechnology(b: GameBoard, deck: PlayerDeck, te: PlayerTechnology, isExecute: Boolean) = {
    val curr = currentPhase(b)
    te.addRound(curr.roundno)
    checkEconomyVictory(b, deck, isExecute)
  }

  def TechnologyUsedAlready(b: GameBoard, te: PlayerTechnology): Boolean = {
    val curr = currentPhase(b)
    te.roundAlready(curr.roundno)
  }

  def canUseTechnology(b: GameBoard, deck: PlayerDeck, tech: TechnologyName.T, command: Command.T): Option[Mess] = {
    val te: Option[PlayerTechnology] = deck.findPlayerTechnology(tech)
    if (te.isEmpty) Some(Mess(message.M.CANNOTFINDTECHNOLOGYINPLAYERDECK, (command, tech)))
    else if (TechnologyUsedAlready(b, te.get))
      Some(Mess(message.M.TECHNOLOGYUSEDALREADYINTHISTURN, (command, te.get)))
    else if (TechnologyFeatures.isCoinTechnology(tech) && te.get.coins >= COINSCAPACITY)
      Some(Mess(message.M.TECHNOLOGYUSEDALREADYINTHISTURN, (command, te.get)))
    else None
  }


  // ================================
  // resources or hut/village
  // ================================

  def resourceForTech(b: GameBoard, tech: TechnologyName.T): Resource.T = b.getTech(tech).resource.get


  def spendResource(b: GameBoard, civ: PlayerDeck, h: HVResource, isExecute: Boolean) = {
    if (h.hv.isEmpty) decrResource(b, civ, h.resource)
    else decrHVResource(b, civ, h)
    if (isExecute && CivilizationFeatures.freeCultureForResourceSpend(civ))
      b.increaseCultureCommand(civ, 1)
  }

  def takeResourceFromBoard(b: GameBoard, deck: PlayerDeck, reso: Resource.T) = {
    deck.resou.incr(reso)
    b.resources.resou.decr(reso)
  }

  def decrResource(b: GameBoard, civ: PlayerDeck, res: Resource.T) = {
    // remove resource from player deck
    civ.resou.decr(res)
    // return resource to the market
    b.resources.resou.incr(res)
  }

  def decrHVResource(b: GameBoard, civ: PlayerDeck, hvres: HVResource) = {
    // hut/village
    // if (hvres.hv.isEmpy find first hut or village with resource
    val hvfound: Option[HutVillage] = civ.hvlist.find(hh => hh.resource == hvres.resource && (hvres.hv.isEmpty || hvres.hv.get == hh.hv))
    if (hvfound.isEmpty)
      throw FatalError(Mess(M.CANNOTFINDHUTVILLAGEINPLAYERDECK, (hvres)))
    // remove from player's resource
    var first: Boolean = true
    // No filterfirst method
    // very ugly, filter out first matching element
    civ.hvlist = civ.hvlist.filter(h => if (first && h == hvfound.get) {
      first = false;
      false
    } else true)
    // return hut/village to board hv used
    b.resources.hvused = b.resources.hvused :+ hvfound.get
  }

  def listOfResources(b: GameBoard, deck: PlayerDeck, resource: Resource.T): Seq[HVResource] = {
    var numofHut: Int = 0
    var numofVil: Int = 0

    deck.hvlist.filter(_.resource == resource).foreach(h => {
      if (h.hv == HutVillage.Hut) numofHut = numofHut + 1
      else numofVil = numofVil + 1
    })
    var res: Seq[HVResource] = Nil
    if (deck.resou.nof(resource) > 0) res = Seq(HVResource(None, resource))
    if (numofHut > 0) res = res :+ HVResource(Some(HutVillage.Hut), resource)
    if (numofVil > 0) res = res :+ HVResource(Some(HutVillage.Village), resource)
    res
  }

  def existResourceAndTech(b: GameBoard, pl: PlayerDeck, res: Resource.T, tech: TechnologyName.T): Boolean = {
    // no resource
    if (!(pl.hvlist.find(_.resource == res).isDefined || pl.resou.nof(res) > 0)) return false
    // check technology
    val te: Option[PlayerTechnology] = pl.tech.find(_.tech == tech)
    // no technology
    if (te.isEmpty) return false
    // used only once
    !TechnologyUsedAlready(b, te.get)
  }

  def decrResourceHVForTech(b: GameBoard, pl: PlayerDeck, res: Resource.T, tech: TechnologyName.T, isExecute: Boolean) = {
    if (pl.resou.nof(res) > 0) decrResource(b, pl, res)
    else decrHVResource(b, pl, HVResource(None, res))
    val te: Option[PlayerTechnology] = pl.tech.find(_.tech == tech)
    if (te.isEmpty)
      throw FatalError(Mess(M.CANNOTFINDTECHNOLOGYINPLAYERDECK, (pl.civ, tech)))
    addCoinToTechnology(b, pl, te.get, isExecute)
  }


  def techologyLevel(b: GameBoard, deck: PlayerDeck): Set[Int] = {
    val trade = numberofTrade(b, deck)
    var tlevel: Int = tradeToLevel(trade.trade)
    if (tlevel == 0) return Set()
    var res: Set[Int] = Set(1);
    // test if there is a room for technology
    while (tlevel > 1) {
      val numtlevel: Int = listofLevel(b, deck, tlevel).length
      val numtlevelp: Int = listofLevel(b, deck, tlevel - 1).length
      if (numtlevel + 1 < numtlevelp) res = res + tlevel // there is a place
      // decrease and check again
      tlevel = tlevel - 1
    }
    res
  }

  def listOfTechnologiesForCiv(b: GameBoard, deck: PlayerDeck, level: Set[Int]): Seq[PlayerTechnology] =
    deck.tech.filter(te => level contains b.techlevel(te))

  def listofLevel(b: GameBoard, deck: PlayerDeck, level: Int): Seq[PlayerTechnology] =
    listOfTechnologiesForCiv(b, deck, Set(level))

  private def remainingTechnologies(b: GameBoard, tech: Seq[Technology], deck: PlayerDeck, level: Set[Int]): Seq[TechnologyName.T] =
    (tech.filter(te => level contains te.level).map(_.tech) toSet) --
      (listOfTechnologiesForCiv(b, deck, level).map(_.tech) toSet) toSeq


  def listOfRemainingTechnologies(b: GameBoard, deck: PlayerDeck, level: Set[Int]): Seq[TechnologyName.T] =
    remainingTechnologies(b, GameResources.instance().tech, deck, level)

  def listOfTechnologiestoLearn(b: GameBoard, deck: PlayerDeck, opposite: PlayerDeck): Seq[TechnologyName.T] =
    remainingTechnologies(b, opposite.tech.map(t => GameResources.getTechnology(t.tech)), deck, techologyLevel(b, opposite))


  // --------------------------------------------------
  // random culture or greatperson
  // --------------------------------------------------

  private def minusE[T](all: Map[T, Int], list: Seq[T]): Seq[T] = {
    // change list of enums to map
    val mlist: Map[T, Int] = list.groupBy(e => e).map(e => e._1 -> e._2.length)
    // minus
    // assuming all values in list are contained in all
    val min: Map[T, Int] = all.map(e => e._1 -> (e._2 - (if (mlist.get(e._1).isDefined) mlist.get(e._1).get else 0)))
    // flatten file and filter already used
    min.filter(_._2 != 0).map(e => Seq.fill(e._2)(e._1)).toSeq flatten
  }


  private def getRandomEnum[T](all: Map[T, Int], list: Seq[T]): T = {
    val rest: Seq[T] = minusE(all, list)
    getRandom(rest)
  }

  def getRandomPerson(b: GameBoard): GreatPersonName.T = {
    val all: Set[GreatPersonName.T] = GameResources.instance().greatpersons.map(_.name) toSet
    val used: Set[GreatPersonName.T] = b.cultureused.persons.toSet
    val allused: Set[GreatPersonName.T] = b.players.foldLeft[Set[GreatPersonName.T]](used)((h, re) => h ++
      re.cultureresource.persons.toSet ++
      re.cultureresource.personsexposed.toSet)
    val available: Set[GreatPersonName.T] = all -- used
    getRandom(available toSeq)
  }

  def getRandomCard(b: GameBoard, level: Int): CultureCardName.Value = {
    val allcards: Map[CultureCardName.Value, Int] = GameResources.instance().culturecards.filter(_.level == level).map(c => c.name -> c.num) toMap
    val allused: Seq[CultureCardName.Value] = b.players.foldLeft[Seq[CultureCardName.Value]](b.cultureused.cards)((h, t) => h ++ t.cultureresource.cards)
    getRandomEnum(allcards, allused)
  }

  // ------------------------------
  // great person gained now
  // ------------------------------

  def isGreatPersonNow(b: GameBoard, civ: Civilization.T): Option[GreatPersonName.T] = {
    val p: Array[Command] = lastPhaseCommandsReverse(b, civ, TurnPhase.CityManagement) toArray

    for (i <- 0 until p.length) {
      if (p(i).command == Command.ADVANCECULTURE && i > 0 && p(i - 1).command == Command.GREATPERSON) {
        val gp: GreatPersonName.T = p(i - 1).param.asInstanceOf[GreatPersonName.T]
        if (i == 1) return Some(gp)
        return None
      }
    }
    return None
  }

  def greatPersonReady(b: GameBoard, deck: PlayerDeck): Seq[GreatPersonName.T] = {
    // great persons available for player
    val gp: Set[GreatPersonName.T] = deck.cultureresource.persons toSet
    // great persons on board
    val p: Set[GreatPersonName.T] = outskirtsForCivNotBlocked(b, deck).filter(_.s.greatperson.isDefined).map(_.s.greatperson.get.name) toSet

    (gp -- p) toSeq
  }


  // -----------------------------------

  def canSaveUnitForCiv(civ: Civilization.T, side: BattleFieldSide): Boolean =
    !side.isvillage && CivilizationFeatures.canSaveUnit(civ) && !side.killed.isEmpty && side.savedunit.isEmpty

  // -------------------
  def getRandomAncientWonder(b: GameBoard): Wonders.T = {
    val wonders: Seq[Wonders.T] = b.getCurrentWonders().map(w => GameResources.getWonder(w)).filter(_.age == WondersAge.Ancient).map(_.name)
    val ra: Wonders.T = getRandom(wonders)
    ra
  }

  // --------------------------
  def advanceCultureForFree(b: GameBoard, civ: Civilization.T, isExecute: Boolean) = {
    if (CivilizationFeatures.advanceCultureWonderCityVillage(civ) && isExecute) {
      val commandC = action.constructCommand(Command.ADVANCECULTUREFORFREE, civ, null)
      b.addForcedCommand(commandC)
    }
  }

  // ----------------------------
  def hasWonderFeature(b: GameBoard, civ: Civilization.T, hasfeature: (Wonders.T) => Boolean): Boolean =
    outskirtsForCivNotBlocked(b, civ).exists(s => s.s.wonder.isDefined && hasfeature(s.s.wonder.get.w))

  // ------------------------------------------
  def destroyCity(b: GameBoard, pl: PlayerDeck, p: P) = {
    squaresAround(b, p).foreach(s => removeStructure(b, s))
    // remove city
    getSquare(b, p).s.city = None
  }

  // ------------------------------------------

  def gameWinner(b: GameBoard): Option[PlayerDeck] =
    b.pllist.map(b.playerDeck(_)).find(_.winthegame.isDefined)

  def checkEconomyVictory(b: GameBoard, deck: PlayerDeck, isExecute: Boolean) =
    if (isExecute && getCoins(b, deck).coins >= ECONOMICVICTORY)
      b.addForcedCommandC(Command.PLAYERWIN, deck, null, Json.toJson(GameWinType.Economic))


}
