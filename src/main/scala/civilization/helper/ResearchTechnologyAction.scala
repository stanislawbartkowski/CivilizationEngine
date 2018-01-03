package civilization.helper

import civilization.action
import civilization.gameboard.{GameBoard, PlayerDeck, PlayerTechnology}
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.action.{AbstractCommand, CommandPackage}
import civilization.helper.HarvestResource.HarvestResource
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization.T
import civilization.objects.Command.T
import play.api.libs.json.JsValue


object ResearchTechnology extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {


  protected class ResearchTechnologyAction(override val param: TechnologyName.T) extends AbstractCommand(param) {

    private def playerTech(deck: PlayerDeck): Set[TechnologyName.T] = deck.tech.map(_.tech.tech).toSet

    private def techLevel(b: GameBoard, tech: TechnologyName.T): Int = b.tech.find(_.tech == tech).get.level

    private def listofLevel(b: GameBoard, deck: PlayerDeck, level: Int): Seq[PlayerTechnology] = deck.tech.filter(_.level == level)

    private def researchTechnologyVerify(b: GameBoard, civ: Civilization.T, tech: TechnologyName.T): Mess = {
      val deck: PlayerDeck = b.playerDeck(civ)
      if (playerTech(deck) contains tech) return Mess(M.TECHNOLOGYRESEARCHEDALREADY, tech)
      val leveltr: Int = levelTrade(techLevel(b, tech))
      val civTrade: Int = numberofTrade(b, civ).trade
      if (leveltr > civTrade) return Mess(M.CANNOTAFFORDTHISTECHNOLOGY, (tech, leveltr, civTrade))
      val level: Int = techLevel(b, tech)
      if (level > 1 && listofLevel(b, deck, level - 1).length + 1 >= listofLevel(b, deck, level).length) return Mess(M.NOPLACEINTECHNOLOGYTREE, tech)
      null
    }

    private def researchTechnologyExecute(b: GameBoard, civ: Civilization.T, tech: TechnologyName.T) = {
      val deck: PlayerDeck = b.playerDeck(civ)
      deck.tech = deck.tech :+ new PlayerTechnology(b.tech.find(_.tech == tech).get)
    }

    override def execute(board: GameBoard) = researchTechnologyExecute(board, civ, param)

    override def verify(board: GameBoard): Mess = researchTechnologyVerify(board, civ, param)

  }

  override def commandsAvail(b: GameBoard, civ: Civilization.T): Seq[Command.T] = {
    val trade = numberofTrade(b,civ)
    if (tradeToLevel(trade.trade)==0) Nil else List(Command.RESEARCH)
  }

  override def getSet: Set[Command.T] = Set(Command.RESEARCH)

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new ResearchTechnologyAction(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = Nil

}