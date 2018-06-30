package civilization.helper

import civilization.action.{AbstractCommand, AbstractCommandNone, Command, CommandPackage}
import civilization.gameboard._
import civilization.helper.GreatPersonAction.{GreatPersonOnBoard, emptyCommand}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{FatalError, M, Mess}
import civilization.objects._
import civilization.{gameboard, message}
import play.api.libs.json.JsValue

object TakeResourceCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set (Command.GETFREERESOURCE,Command.TAKEFREEALLRESOURCESFROMMARKET)

  protected class TakeFreeResources extends AbstractCommandNone {

    override def verify(board: gameboard.GameBoard): message.Mess = null

    override def execute(board: gameboard.GameBoard): Unit = {
      if (!CivilizationFeatures.freeResourcesAtStart(civ))
        throw FatalError(Mess(M.YOUARENOTENTITLETORECEIVEFREERESOURCES, civ))
      if (!firstRound(board,Some(TurnPhase.StartOfTurn)))
        throw FatalError(Mess(M.FREERESOURCESONLYATTHEBEGINNINGOFTHEGAME, civ))
      Resource.values.filter(Resource.isMarketResource(_)).foreach(r => {
        takeResourceFromBoard(board, deck, r)
      })
    }
  }


  protected class TakeResourceCommand (override val param: Resource.T) extends AbstractCommand(param) {
    override def verify(board: GameBoard): Mess = {
      if (deck.takefreeResources == 0) return Mess(M.CANNOTTAKEFREERESOURCE)
      if (board.resources.resou.nof(param) == 0) return Mess(M.RESOURCENOTAVAILABLE,param)
      null
    }

    override def execute(board: GameBoard): Unit = {
      assert(deck.takefreeResources > 0)
      deck.takefreeResources = deck.takefreeResources - 1
      takeResourceFromBoard(board, deck, param)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command =
    if (command == Command.GETFREERESOURCE) new TakeResourceCommand(param) else new TakeFreeResources()

  override def itemize(b: GameBoard, deck : PlayerDeck, com: Command.T): Seq[JsValue] = {
    val res: Seq[JsValue] =
      if (deck.takefreeResources == 0) Nil
      else Resource.values.toSeq.filter(r => b.resources.resou.nof(r) > 0)
    // reset takeresourcecounter if not available
    if (res.isEmpty) deck.takefreeResources == 0
    res
  }
}

