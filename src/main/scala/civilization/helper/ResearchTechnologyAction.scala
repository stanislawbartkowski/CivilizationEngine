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

  private def applyNewStrength(str : CombatUnitStrength,t : CombatUnitType.T,newval : Int) =
    str.setStrength(t,math.max(str.getStrength(t),newval))

  private def upgradeMilitary(str : CombatUnitStrength, tech : Technology): Option[CombatUnitType.T] = {
    if (tech.unit.isEmpty) return None
    val s : TechnologyUnit = tech.unit.get
    val newlevel : Int = s.level-1
    if (s.unit.isDefined) {
      if (newlevel > str.getStrength(s.unit.get)) {
        str.setStrength(s.unit.get,newlevel)
        return Some(s.unit.get)
      }
    }
    else
    // apply new level to all units
      CombatUnitType.values.foreach(applyNewStrength(str,_,newlevel))
    None
  }


  private def upgradeB(b: GameBoard, civ : Civilization.T, t: Technology) : Unit = {
    // technology do not unlock any building
    if (t.building.isEmpty) return
    citiesForCivilization(b,civ).foreach(
      c => {
        // buildings
        squaresAround(b,c.p).filter(_.s.building.isDefined).foreach( ss => {
          val bui : Building = GameResources.getBuilding(ss.s.building.get.name)
          if (bui.upgrade.isDefined)
            if (bui.upgrade.get == t.building.get) ss.s.setBuilding(t.building.get)
        })
      }
    )
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

    private def researchTechnologyExecute(b: GameBoard, civ: Civilization.T, tech: TechnologyName.T) = {
      val deck: PlayerDeck = b.playerDeck(civ)
      deck.tech = deck.tech :+ new PlayerTechnology(tech)
      // upgrade buildings
      val t : Technology = GameResources.getTechnology(tech)
      upgradeB(b,civ,t)
      // upgrade military strength
      val upgrade : Option[CombatUnitType.T] = upgradeMilitary(deck.combatlevel,t)
      if (upgrade.isDefined && CivilizationFeatures.takefreeResourceAfterUpgradingMilitary(civ)) {
        deck.takefreeResources = true
        if (isExecute) {
          val commandC = constructCommand(Command.TAKEUNIT, civ, null, getRandomUnit(b, upgrade.get, false))
          //          playCommand(g, commandC)
          b.addForcedCommand(commandC)
        }
      }
    }

    override def execute(board: GameBoard) = researchTechnologyExecute(board, civ, param)

    override def verify(board: GameBoard): Mess = researchTechnologyVerify(board, civ, param)

  }

  override def commandsAvail(b: GameBoard, civ: Civilization.T,phase: TurnPhase.T): Seq[Command.T] =
    if (isResearchDone(b, civ) || techologylevel(b, civ) == 0) Nil else List(Command.RESEARCH)


  override def getSet: Set[Command.T] = Set(Command.RESEARCH)

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new ResearchTechnologyAction(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = Nil

}