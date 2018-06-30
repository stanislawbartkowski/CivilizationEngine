package civilization.helper

import civilization.action._
import civilization.{gameboard, message}
import civilization.message._
import civilization.gameboard.{BoardResources, GameBoard, PlayerDeck}
import civilization.helper.IncreaseTradeAction.IncreaseTradeAction
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Command, _}
import play.api.libs.json._

object SpendSilkAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.USESILKFORTRADE9)

  private def listOfRes(b: gameboard.GameBoard, deck: PlayerDeck): Seq[HVResourceCiv] = {
    if (!existResourceAndTech(b, deck, Resource.Silk, TechnologyName.HorsebackRiding)) return Nil
    if (commandUsedAlready(b,deck.civ,TurnPhase.Trade,Command.USESILKFORTRADE9)) return Nil
    // prepare list of resources
    val list : Seq[HVResource] = listOfResources(b,deck,Resource.Silk)
    if (b.pllist.length == 1)
      list.map(HVResourceCiv(_,None))
    else
      b.pllist.filter(_ != deck.civ).flatMap(c => list.map(HVResourceCiv(_,Some(c))))
  }

  protected class SpendSilkAction(override val param: HVResourceCiv) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      if (listOfRes(board, deck).exists(_ == param)) return null
      Mess(M.CANNOTSPENDSILKFORTRADE, (command, param))
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      board.addForcedCommandC(Command.INCREASETRADE, civ, null, JsNumber(9))
      if (board.pllist.length > 1) {
        if (param.civ.isEmpty) throw new FatalError(Mess(M.TOSPENDSILKONTRADEYOUHAVETOPINTOTHERCIV))
        if (civ == param.civ.get) throw new FatalError(Mess(M.YOUCANNOTSPENDSECONDSILKONYOUSELF))
        board.addForcedCommandC(Command.INCREASETRADE, param.civ.get, null,JsNumber(6))
      }
    }
  }

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = listOfRes(b, deck)

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new SpendSilkAction(param)

}
