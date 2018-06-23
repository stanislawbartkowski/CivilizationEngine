package civilization.helper

import civilization.action
import civilization.gameboard.{GameBoard, PlayerDeck, PlayerTechnology}
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.action.{AbstractCommand, CommandPackage, constructCommand}
import civilization.helper.HarvestResource.HarvestResource
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GameResources
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization.T
import civilization.objects.Command.T
import play.api.libs.json.JsValue


object ResearchTechnology extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.RESEARCH, Command.RESEARCHFREETECHNOLOGY)

  private def applyNewStrength(str: CombatUnitStrength, t: CombatUnitType.T, newval: Int) =
    str.setStrength(t, math.max(str.getStrength(t), newval))

  private def upgradeMilitary(str: CombatUnitStrength, tech: Technology): Seq[CombatUnitType.T] = {
    if (tech.unit.isEmpty) return Nil
    val s: TechnologyUnit = tech.unit.get
    val newlevel: Int = s.level - 1
    // single unit or list of 3 units
    // filter out only with increased strength
    val listofunlocked: Seq[CombatUnitType.T] =
    (if (s.unit.isDefined) Seq(s.unit.get)
    else CombatUnitType.values.filter(_ != CombatUnitType.Artillery) toSeq).filter(newlevel > str.getStrength(_))

    // increase strength
    listofunlocked.foreach(str.setStrength(_, newlevel))
    // list of unlocked
    listofunlocked
  }


  private def upgradeB(b: GameBoard, civ: Civilization.T, t: Technology): Unit = {
    // technology do not unlock any building
    if (t.building.isEmpty) return
    citiesForCivilization(b, civ).foreach(
      c => {
        // buildings
        squaresAround(b, c.p).filter(_.s.building.isDefined).foreach(ss => {
          val bui: Building = GameResources.getBuilding(ss.s.building.get.name)
          if (bui.upgrade.isDefined)
            if (bui.upgrade.get == t.building.get) ss.s.setBuilding(t.building.get)
        })
      }
    )
  }

  private def researchTechnologyExecute(b: GameBoard, deck: PlayerDeck, tech: TechnologyName.T, isExecute: Boolean) = {
    deck.tech = deck.tech :+ new PlayerTechnology(tech)
    // upgrade buildings
    val t: Technology = GameResources.getTechnology(tech)
    upgradeB(b, deck.civ, t)
    // upgrade military strength
    val listofunlocked: Seq[CombatUnitType.T] = upgradeMilitary(deck.combatlevel, t)
    deck.takefreeResources = 0
    if (CivilizationFeatures.takefreeResourceAfterUpgradingMilitary(deck.civ))
      listofunlocked.foreach(u => {
        deck.takefreeResources = deck.takefreeResources + 1
        if (isExecute) {
          val commandC = constructCommand(Command.TAKEUNIT, deck.civ, null, getRandomUnit(b, u, false))
          //          playCommand(g, commandC)
          b.addForcedCommand(commandC)
        }
      })
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
      if (level > 1 && listofLevel(b, deck, level - 1).length + 1 <= listofLevel(b, deck, level).length) return Mess(M.NOPLACEINTECHNOLOGYTREE, tech)
      null
    }

    override def execute(board: GameBoard) = researchTechnologyExecute(board, deck, param, isExecute)

    override def verify(board: GameBoard): Mess = researchTechnologyVerify(board, civ, param)
  }

  protected class ResearchFreeTechnologyAction(override val param: TechnologyName.T) extends AbstractCommand(param) {
    override def execute(board: GameBoard) = researchTechnologyExecute(board, deck, param, isExecute)

    override def verify(board: GameBoard): Mess = null
  }

  override def commandsAvail(b: GameBoard, deck: PlayerDeck, phase: TurnPhase.T): Seq[Command.T] =
    if (isResearchDone(b, deck) || techologyLevel(b, deck) == 0) Nil else List(Command.RESEARCH)


  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) =
    if (command == Command.RESEARCH) new ResearchTechnologyAction(param)
    else new ResearchFreeTechnologyAction(param)

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] =
    if (com == Command.RESEARCH) listOfRemainingTechnologiesUpTo(b, deck, techologyLevel(b, deck))
    else Nil

}