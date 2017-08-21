package civilization.helper

import civilization.action.AbstractCommand
import civilization.gameboard.{GameBoard, PlayerDeck, PlayerTechnology}
import civilization.message.{M, Mess}
import civilization.objects._

class ResearchTechnologyAction(override val param: TechnologyName.T) extends AbstractCommand(param) {

  private def playerTech(deck: PlayerDeck): Set[TechnologyName.T] = deck.tech.map(_.tech).toSet

  private def techLevel(b: GameBoard, tech: TechnologyName.T): Int = b.tech.find(_.tech == tech).get.level

  private def listofLevel(b: GameBoard, deck: PlayerDeck, level: Int): Seq[PlayerTechnology] = deck.tech.filter(t => techLevel(b, t.tech) == level)

  private def researchTechnologyVerify(b: GameBoard, civ: Civilization.T, tech: TechnologyName.T): Mess = {
    val deck: PlayerDeck = b.playerDeck(civ)
    if (playerTech(deck) contains tech) return Mess(M.TECHNOLOGYRESEARCHEDALREADY, tech)
    val leveltr: Int = levelTrade(techLevel(b, tech))
    val civTrade: Int = numberofTrade(b, civ)
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
