package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard.{BuildingPoint, GameBoard, PlayerDeck, PlayerTechnology}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GameResources
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.{action, gameboard, message, objects}
import civilization.objects._
import play.api.libs.json.JsValue


trait TechnologyAnyResourceAction extends CommandPackage with ResourceActionTrait with ImplicitMiximFromJson with ImplicitMiximToJson {

  def validateH(b: GameBoard, deck: PlayerDeck, command: Command.T): Option[Mess]

  def executeI(board: gameboard.GameBoard, deck: PlayerDeck, isExecute : Boolean): Unit

  private def canuse(b: GameBoard, deck: PlayerDeck, command: Command.T): Option[Mess] = {
    def resany: Int = resnum(command)
    // not enough resources
    if (numofAnyResources(b, deck) < resany)
      return Some(Mess(message.M.NOTENOUGHRESOURCETOUSETECHNOLOGY, (command, techn)))
    val m: Option[Mess] = canUseTechnology(b, deck, techn, command)
    if (m.isDefined) m
    else
      validateH(b, deck, command)
  }


  private def numofAnyResources(b: GameBoard, pl: PlayerDeck): Int = {
    // sum of resource and hut/villages
    pl.resou.table.filter(r => r._1 != Resource.Coin && r._1 != Resource.Culture).map(_._2).sum + pl.hvlist.length
  }

  private def resnum(command: Command.T): Int =
    GameResources.getTechnology(techn).resourceany.get

  protected class TechnologyAnyResourceAction(override val param: Seq[HVResource]) extends AbstractCommand(param) {

    override def execute(board: gameboard.GameBoard): Unit = {
      executeI(board, deck,isExecute)
      // remove hut and villages
      param.foreach(spendResource(board, deck, _, isExecute))
    }

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val m: Option[Mess] = canuse(board, deck, command)
      if (m.isEmpty) null else m.get
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new TechnologyAnyResourceAction(param)

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] =
    if (canuse(b, deck, com).isDefined) Nil
    else emptyItemize()
}
