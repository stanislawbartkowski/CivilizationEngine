package civilization.helper

import civilization.action.{AbstractCommandNone, CommandPackage}
import civilization.{action, gameboard, message}
import civilization.gameboard.{GameBoard, PlayerDeck, PlayerTechnology}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import play.api.libs.json.JsValue

trait CoinActionTrait extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  val command: Command.T
  val tech: TechnologyName.T

  override def getSet: Set[Command.T] = Set(command)

  def validateH(b: GameBoard, deck: PlayerDeck, command: Command.T): Option[Mess]

  def executeI(board: gameboard.GameBoard, deck: PlayerDeck): Unit

  private def canuse(b: GameBoard, deck: PlayerDeck, command: Command.T): Option[Mess] = {
    val m: Option[Mess] = canUseTechnology(b, deck, tech, command)
    if (m.isDefined) m
    else
      validateH(b, deck, command)
  }

  protected class CoinAction extends AbstractCommandNone {

    override protected def verify(board: GameBoard): Mess = {
      val m: Option[Mess] = canuse(board, deck, command)
      if (m.isEmpty) null else m.get
    }

    override protected def execute(board: GameBoard): Unit = {
      val te: PlayerTechnology = deck.findPlayerTechnology(tech).get
      addCoinToTechnology(board, deck, te, isExecute)
      if (isExecute) executeI(board, deck)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): action.Command = new CoinAction()

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] =
    if (canuse(b, deck, com).isDefined) Nil
    else emptyItemize()
}
