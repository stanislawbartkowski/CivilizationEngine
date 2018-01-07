package civilization.helper

import civilization.action
import civilization.gameboard.{GameBoard, PlayerDeck, PlayerTechnology}
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.action.{AbstractCommand, CommandPackage}
import civilization.helper.HarvestResource.HarvestResource
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GameResources
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization.T
import civilization.objects.Command.T
import play.api.libs.json.JsValue


object ResearchTechnology extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {


  private def listofLevel(b: GameBoard, deck: PlayerDeck, level: Int): Seq[PlayerTechnology] = deck.tech.filter(b.techlevel(_) == level)

  def techologylevel(b: GameBoard, civ: Civilization.T): Int = {
    val trade = numberofTrade(b, civ)
    var tlevel: Int = tradeToLevel(trade.trade)
    if (tlevel == 0) return 0
    // test if there is a room for technology
    val deck: PlayerDeck = b.playerDeck(civ)
    while (tlevel > 1) {
      val numtlevel: Int = listofLevel(b, deck, tlevel).length
      val numtlevelp: Int = listofLevel(b, deck, tlevel - 1).length
      if (numtlevel + 1 < numtlevelp) return tlevel // there is a place
      // decrease and check again
      tlevel = tlevel - 1
    }
    return 1 // first level
  }


  protected class ResearchTechnologyAction(override val param: TechnologyName.T) extends AbstractCommand(param) {

    private def playerTech(deck: PlayerDeck): Set[TechnologyName.T] = deck.tech.map(_.tech).toSet

    private def techLevel(b: GameBoard, tech: TechnologyName.T): Int = GameResources.instance().tech.find(_.tech == tech).get.level

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
      deck.tech = deck.tech :+ new PlayerTechnology(tech)
    }

    override def execute(board: GameBoard) = researchTechnologyExecute(board, civ, param)

    override def verify(board: GameBoard): Mess = researchTechnologyVerify(board, civ, param)

  }

  override def commandsAvail(b: GameBoard, civ: Civilization.T): Seq[Command.T] =
    if (isResearchDone(b, civ) || techologylevel(b, civ) == 0) Nil else List(Command.RESEARCH)


  override def getSet: Set[Command.T] = Set(Command.RESEARCH)

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new ResearchTechnologyAction(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = Nil

}