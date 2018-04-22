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

//  override def getSet: Set[Command.T] = Set(Command.POTTERYACTION, Command.PHILOSOPHYACTION)
  override def getSet: Set[Command.T] = Set(Command.POTTERYACTION)

  private def increaseCoins(tp: PlayerTechnology) = {
    // increase number of coins
    tp.coins = if (tp.coins.isEmpty) Some(1) else Some(tp.coins.get + 1)
  }

  private def removeResource(b: GameBoard, civ: Civilization.T, h: HVResource) = {
    val pl: PlayerDeck = b.playerDeck(civ)
    if (h.hv.isEmpty) {
      // resource only
      // remove resource from player deck
      pl.resou.decr(h.resource)
      // return resource to the market
      b.resources.resou.incr(h.resource)
    }
    else {
      // hut/village
      var firstfound = false
      pl.hvlist = pl.hvlist.filter(hh => firstfound || (hh.hv != h.hv.get || hh.resource != h.resource || {
        firstfound = true;
        false
      }))
      // return hut/village to board hv used
      b.resources.hvused = b.resources.hvused :+ HutVillage(h.hv.get, h.resource)
    }
  }

  private def numofAnyResources(b: GameBoard, civ: Civilization.T): Int = {
    val pl: PlayerDeck = b.playerDeck(civ)
    // sum of resource and hut/villages
    pl.resou.table.filter(r => r._1 != Resource.Coin && r._1 != Resource.Culture).map(_._2).sum +
      pl.hvlist.length
  }

  private def technology(command: Command.T): TechnologyName.T = if (command == Command.POTTERYACTION) TechnologyName.Pottery else TechnologyName.Philosophy

  private def resnum(command: Command.T): Int =
    GameResources.getTechnology(technology(command)).resourceany.get

  private def findpltechnology(b: GameBoard, civ: Civilization.T, com: Command.T): Option[PlayerTechnology] = {
    val pl: PlayerDeck = b.playerDeck(civ)

    def t: TechnologyName.T = technology(com)
    // technology researched
    pl.tech.find(_.tech == t)
  }


  private def getListOfCities(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[P] = {
    // technology researched
    def tp: Option[PlayerTechnology] = findpltechnology(b, civ, com)

    if (tp.isEmpty) return Nil
    // check capacity on technology
    if (tp.get.coins.isDefined && tp.get.coins.get >= COINSCAPACITY) return Nil

    def resany: Int = resnum(com)
    // not enough resources
    if (numofAnyResources(b, civ) < resany) return Nil
    // cities available for city action
    CityAvailableForAction(b, civ)
  }

  protected class PotteryPhilosophyAction(override val param: Seq[HVResource]) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val m: Seq[P] = getListOfCities(board, civ, command)
      if (m.find(_ == p).isDefined) return null
      Mess(M.INVALIDTECHNOLOGYACTION, (command, param))
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      def tp: PlayerTechnology = findpltechnology(board, civ, command).get
      // increase number of coins
      increaseCoins(tp)
      // remove hut and villages
      param.foreach(removeResource(board, civ, _))
    }
  }


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new PotteryPhilosophyAction(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = getListOfCities(b, civ, com)

}