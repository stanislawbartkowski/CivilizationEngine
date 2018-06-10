package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard._
import civilization.helper.GreatPersonAction.{GreatPersonOnBoard, emptyCommand}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}
import play.api.libs.json.JsValue

object TakeResourceCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set (Command.GETFREERESOURCE)

  protected class TakeResourceCommand (override val param: Resource.T) extends AbstractCommand(param) {
    override def verify(board: GameBoard): Mess = {
      val pl : PlayerDeck = board.playerDeck(civ)
      if (!pl.takefreeResources) return Mess(M.CANNOTTAKEFREERESOURCE)
      if (board.resources.resou.nof(param) == 0) return Mess(M.RESOURCENOTAVAILABLE,param)
      null
    }

    override def execute(board: GameBoard): Unit = {
      val pl : PlayerDeck = board.playerDeck(civ)
      pl.takefreeResources = false
      takeResourceFromBoard(board, civ, param)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new TakeResourceCommand(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] =
    if (!b.playerDeck(civ).takefreeResources) Nil
    else Resource.values.toSeq.filter(r => b.resources.resou.nof(r) > 0)

}

