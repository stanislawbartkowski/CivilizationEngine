package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects._
import civilization.objects.Command.T
import play.api.libs.json.JsValue
import civilization.action._
import civilization.gameboard._
import civilization.{gameboard, message}
import civilization.gameboard.CultureTrack._
import civilization.io.readdir._
import civilization.message.{FatalError, M, Mess}

object AdvanceCulture extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[T] = Set(Command.ADVANCECULTURE)

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
      // increase culture
      pl.cultureprogress = pl.cultureprogress + 1
    }
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new AdvanceCulture

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = {
    val c: Option[CultureTrackCost] = advanceCulture(b, civ)
    if (c.isEmpty) Nil
    else writeCultureCost(Seq(c.get))
  }


}
