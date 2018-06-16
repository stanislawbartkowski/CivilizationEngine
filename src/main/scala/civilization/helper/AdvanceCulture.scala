package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage, _}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects._
import civilization.objects.Command.T
import play.api.libs.json.JsValue
import civilization.gameboard._
import civilization.{gameboard, message}
import civilization.gameboard.CultureTrack._
import civilization.io.readdir._
import civilization.message.{FatalError, M, Mess}

object AdvanceCulture extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[T] = Set(Command.GET3CULTURE, Command.GETCULTURE, Command.ADVANCECULTURE, Command.CULTURECARD, Command.GREATPERSON, Command.ADVANCECULTUREFORFREE)

  private def getCultureCost(cult: Int): CultureTrackCost = {
    val culturetrack: CultureTrack = GameResources.instance().culturetrack
    for (i <- 0 until culturetrack.length)
      if (cult <= culturetrack(i).last) return culturetrack(i).cost
    val last = culturetrack.length - 1
    if (cult == culturetrack(last).last + 1) return culturetrack(last).cost
    throw FatalError(Mess(M.CULTURELEVELEXCCEED, (cult, culturetrack(last).last + 1)))
  }

  private def advanceCulture(b: GameBoard, civ: Civilization.T): Option[CultureTrackCost] = {
    val pl: PlayerDeck = b.playerDeck(civ)
    val cost: CultureTrackCost = getCultureCost(pl.cultureprogress + 1)
    // verify if can afford
    if (cost.culture > pl.resou.nof(Resource.Culture)) return None
    val trade: TradeForCiv = numberofTrade(b, civ)
    if (cost.trade > trade.trade) return None
    Some(cost)
  }

  private def isGreatPerson(cult: Int): Boolean = {
    val culturetrack: CultureTrack = GameResources.instance().culturetrack
    for (i <- 0 until culturetrack.length)
      if (cult <= culturetrack(i).last)
        return (culturetrack(i).greatperson.find(_ == cult)).isDefined
    false
  }

  private def cultureLevel(cult: Int): Int = {
    val culturetrack: CultureTrack = GameResources.instance().culturetrack
    for (i <- 0 until culturetrack.length)
      if (cult <= culturetrack(i).last)
        return i + 1
    // TODO: throws exception
    return 0
  }

  private def advanceCulture(board: gameboard.GameBoard, pl: PlayerDeck, isExecute: Boolean) = {
    pl.cultureprogress = pl.cultureprogress + 1
    if (isExecute) {
      val commandC: Command = if (isGreatPerson(pl.cultureprogress)) constructCommand(Command.GREATPERSON, pl.civ, null, getRandomPerson(board))
      else constructCommand(Command.CULTURECARD, pl.civ, null, getRandomCard(board, cultureLevel(pl.cultureprogress)))
      board.addForcedCommand(commandC)
    }
  }


  protected class AdvanceCulture extends AbstractCommandNone {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val c: Option[CultureTrackCost] = advanceCulture(board, civ)
      if (c.isDefined) return null
      Mess(M.CANNOTAFFORDADVANCECULTURE)
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      val cul: CultureTrackCost = advanceCulture(board, civ).get
      val pl: PlayerDeck = board.playerDeck(civ)
      pl.resou.decr(Resource.Culture, cul.culture)
      // keep culture cost for future deduction of trade
      param1 = cul
      advanceCulture(board, pl, isExecute)
    }
  }

  protected class TakeCultureCard(override val param: CultureCardName.T) extends AbstractCommand(param) {
    override def verify(board: GameBoard): Mess = null

    override def execute(board: GameBoard): Unit =
      board.playerDeck(civ).cultureresource.cards = board.playerDeck(civ).cultureresource.cards :+ param
  }

  protected class TakeGreatPerson(override val param: GreatPersonName.T) extends AbstractCommand(param) {
    override def verify(board: GameBoard): Mess = null

    override def execute(board: GameBoard): Unit =
      board.playerDeck(civ).cultureresource.persons = board.playerDeck(civ).cultureresource.persons :+ param
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) =
    command match {
      case Command.ADVANCECULTURE => new AdvanceCulture()
      case Command.ADVANCECULTUREFORFREE => new AbstractCommandNone() {
        override def verify(board: GameBoard): Mess = null

        override def execute(board: GameBoard): Unit = {
          val pl: PlayerDeck = board.playerDeck(civ)
          advanceCulture(board, pl, isExecute)
        }
      }
      case Command.CULTURECARD => new TakeCultureCard(param)
      case Command.GREATPERSON => new TakeGreatPerson(param)
      case Command.GET3CULTURE | Command.GETCULTURE => new AbstractCommandNone() {
        override def verify(board: GameBoard): Mess = null

        override def execute(board: GameBoard): Unit = {
          val pl: PlayerDeck = board.playerDeck(civ)
          pl.resou.incr(Resource.Culture, if (command == Command.GET3CULTURE) 3 else 1)
        }
      }
    }

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = {
    val c: Option[CultureTrackCost] = advanceCulture(b, civ)
    if (c.isEmpty) Nil
    else writeCultureCost(Seq(c.get))
  }


}
