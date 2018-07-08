package civilization.objects

object TechnologyFeatures {

  def increaseHandSize(tech: TechnologyName.T): Boolean = (tech == TechnologyName.Pottery)

  def citiesLimitIs3(tech: TechnologyName.T): Boolean = tech == TechnologyName.Irrigation

  def watercrossingAllowed(tech: TechnologyName.T): Boolean = (tech == TechnologyName.Navigation || tech == TechnologyName.Sailing)

  def canStopInWater(tech: TechnologyName.T): Boolean = tech == TechnologyName.Sailing

  def stackSize(tech: TechnologyName.T): Int = if (tech == TechnologyName.Masonry) 3 else 0 /* stackSize here is taken from CivilizationFeatures */

  def buyCityWall(tech: TechnologyName.T): Boolean = tech == TechnologyName.Masonry

  def speedLimit(tech: TechnologyName.T): Int = if (tech == TechnologyName.Sailing) 4 else if (tech == TechnologyName.HorsebackRiding) 3 else 0

  def buyFigureInAnyCityWithShipyard(tech: TechnologyName.T): Boolean = tech == TechnologyName.Navy

}
