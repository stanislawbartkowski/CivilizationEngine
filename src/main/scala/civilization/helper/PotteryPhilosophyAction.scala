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

object PotteryPhilosophyAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.POTTERYACTION)

  private def numofAnyResources(b: GameBoard, pl: PlayerDeck): Int = {
    // sum of resource and hut/villages
    pl.resou.table.filter(r => r._1 != Resource.Coin && r._1 != Resource.Culture).map(_._2).sum + pl.hvlist.length
  }

  private def technology(command: Command.T): TechnologyName.T = if (command == Command.POTTERYACTION) TechnologyName.Pottery else TechnologyName.Philosophy

  private def resnum(command: Command.T): Int =
    GameResources.getTechnology(technology(command)).resourceany.get

  private def findpltechnology(b: GameBoard, pl: PlayerDeck, com: Command.T): Option[PlayerTechnology] = {

    def t: TechnologyName.T = technology(com)
    // technology researched
    pl.tech.find(_.tech == t)
  }


  private def getListOfCities(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[P] = {
    // technology researched
    def tp: Option[PlayerTechnology] = findpltechnology(b, deck, com)

    if (tp.isEmpty) return Nil
    // only once per turn
    if (TechnologyUsedAlready(b, tp.get)) return Nil
    // check capacity on technology
    if (tp.get.coins >= COINSCAPACITY) return Nil

    def resany: Int = resnum(com)
    // not enough resources
    if (numofAnyResources(b, deck) < resany) return Nil
    // cities available for city action
    CityAvailableForAction(b, deck.civ)
  }

  protected class PotteryPhilosophyAction(override val param: Seq[HVResource]) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val m: Seq[P] = getListOfCities(board, deck, command)
      if (m.find(_ == p).isDefined) return null
      Mess(M.INVALIDTECHNOLOGYACTION, (command, param))
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      def tp: PlayerTechnology = findpltechnology(board, deck, command).get
      // increase number of coins
      addCoinToTechnology(board, tp)
      // remove hut and villages
      param.foreach(spendResource(board, deck, _, isExecute))
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new PotteryPhilosophyAction(param)

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = getListOfCities(b, deck, com)

}
