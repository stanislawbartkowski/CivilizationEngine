package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard._
import civilization.gameboard.GameBoard
import civilization.helper.DevoutToCultureCommand.DevoutToCultureCommand
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message
import civilization.message.{M, Mess}
import civilization.objects.{Civilization, Command, CultureCardName, P}
import play.api.libs.json.JsValue

object DiscardCard extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.DISCARDCARD)

  class DiscardCardCommand(override val param: CultureCardName.T) extends AbstractCommand(param) {
    override def verify(board: GameBoard): message.Mess = {
      val pl: PlayerDeck = board.playerDeck(civ)
      if (pl.cultureresource.cards.find(_ == param).isDefined) return null
      return new Mess(M.CULTURECARDNOTINPLAYERHAND,param)
    }

    override def execute(board: GameBoard): Unit = {
      val pl: PlayerDeck = board.playerDeck(civ)
      // remove from player deck
      pl.cultureresource.cards = pl.cultureresource.cards.filter(_ != param)
      // add to board as used
      board.cultureused.cards = board.cultureused.cards :+ param
    }
  }

  override def itemize(b: GameBoard, deck : PlayerDeck, com: Command.T): Seq[JsValue] = {
    val limits: PlayerLimits = getLimits(b, deck.civ)
    if (deck.cultureresource.cards.length <= limits.handsize) return Nil
    deck.cultureresource.cards
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new DiscardCardCommand(param)

}
