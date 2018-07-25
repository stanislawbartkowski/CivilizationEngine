package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard.{BuildingPoint, GameBoard, PlayerDeck}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.{action, gameboard, message, objects}
import civilization.objects.Civilization.T
import civilization.objects.{Civilization, Command, P}
import play.api.libs.json.JsValue

object DevoutToCultureCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  private def itemizeCommandsForCulture(b: gameboard.GameBoard, civ: Civilization.T): Seq[(P, Seq[P])] = {
    val cityS: Seq[(P, P)] = scoutsAvailableForAction(b, civ, (sc) => sc.culture > 0)
    val g: Seq[(P, Seq[(P, P)])] = cityS.groupBy(_._1).toSeq
    val li: Seq[(P, Seq[P])] = g.map(s => (s._1, s._2.map(_._2)))
    val liS: Set[P] = li.map(_._1) toSet
    // add cities without scouts
    val cityE: Seq[P] = CityAvailableForAction(b, civ).filter(c => !(liS contains c))
    li ++ cityE.map(s => (s, Nil))
  }

  class DevoutToCultureCommand(override val param: Seq[P]) extends AbstractCommand(param) {
    override def verify(board: gameboard.GameBoard): message.Mess = {
      val s: Seq[(P, Seq[P])] = itemizeCommandsForCulture(board, civ)
      val city: Option[(P, Seq[P])] = s.find(_._1 == p)
      if (city.isEmpty) return new Mess(M.CANNOTDEVOUTTHICCITYFORCULTURE, param)
      val pset: Set[P] = param toSet
      val allowedset: Set[P] = city.get._2.toSet
      //      val allowedset : Set[P] = Set()
      // if allowdset contains pset
      if (pset subsetOf allowedset) return null
      return new Mess(M.CANNOTSENTCLUTUREFROMSCOUT, param)
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      // single culture from every scout
      val culture = cultureForCity(board, p).culture + param.length
      // increase culture
      deck.resou.incr(objects.Resource.Culture, culture)
    }
  }

  override def getSet: Set[Command.T] = Set(Command.DEVOUTTOCULTURE)

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new DevoutToCultureCommand(param)

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = itemizeCommandsForCulture(b, deck.civ)

}
