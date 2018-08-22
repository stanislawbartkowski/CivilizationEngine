package civilization.objects

object TechnologyFeatures {

  def increaseHandSize(tech: TechnologyName.T): Boolean = (tech == TechnologyName.Pottery)

  def citiesLimitIs3(tech: TechnologyName.T): Boolean = tech == TechnologyName.Irrigation

  def watercrossingAllowed(tech: TechnologyName.T): Boolean = (tech == TechnologyName.Navigation || tech == TechnologyName.Sailing)

  def canStopInWater(tech: TechnologyName.T): Boolean = tech == TechnologyName.Sailing

  def stackSize(tech: TechnologyName.T): Int = if (tech == TechnologyName.Masonry) 3 else if (tech == TechnologyName.ReplaceableParts) 6 else 0 /* stackSize here is taken from CivilizationFeatures */

  def buyCityWall(tech: TechnologyName.T): Boolean = tech == TechnologyName.Masonry

  def speedLimit(tech: TechnologyName.T): Int = if (tech == TechnologyName.Sailing) 4 else if (tech == TechnologyName.HorsebackRiding) 3 else 0

  def buyFigureInAnyCityWithShipyard(tech: TechnologyName.T): Boolean = tech == TechnologyName.Navy

  def isCoinTechnology(tech: TechnologyName.T): Boolean = (tech == TechnologyName.Pottery || tech == TechnologyName.CodeOfLaw || tech == TechnologyName.Democracy || tech == TechnologyName.PrintingPress)

  def increaseProdForCoins(tech: TechnologyName.T): Boolean = tech == TechnologyName.MilitaryScience

  private val rescommT : Map[Command.T,TechnologyName.T] = Map(Command.POTTERYACTION -> TechnologyName.Pottery,
    Command.PHILOSOPHYACTION -> TechnologyName.Philosophy, Command.CURRENCYACTION -> TechnologyName.Currency,
    Command.CONSTRUCTIONACTION -> TechnologyName.Construction, Command.METALCASTINGACTION -> TechnologyName.MetalCasting,
    Command.BANKINGACTION -> TechnologyName.Banking, Command.CHIVALRYACTION ->TechnologyName.Chivalry,
    Command.DEMOCRACYACTION ->  TechnologyName.Democracy, Command.PRINTINGPRESSACTION -> TechnologyName.PrintingPress)


  //def isTechnologyResourceAction(t: Value): Boolean =
  //  t == POTTERYACTION || t == PHILOSOPHYACTION || t == CURRENCYACTION || t == CONSTRUCTIONACTION || t == METALCASTINGACTION || t == BANKINGACTION || t == CHIVALRYACTION || t == DEMOCRACYACTION || t == PRINTINGPRESSACTION


  def commandResourceTechnology(comm : Command.T) : Option[TechnologyName.T] = rescommT.get(comm)

  def isNotResourceAbilityAction(comm: Command.T) : Boolean = commandResourceTechnology(comm).isEmpty
}
