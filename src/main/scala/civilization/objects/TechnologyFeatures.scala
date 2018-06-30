package civilization.objects

object TechnologyFeatures {

  def increaseHandSize(tech : TechnologyName.T) : Boolean = (tech == TechnologyName.Pottery)

  def citiesLimitIs3(tech : TechnologyName.T) : Boolean = tech == TechnologyName.Irrigation

  def watercrossingAllowed(tech : TechnologyName.T) : Boolean = tech == TechnologyName.Navigation

  def stackSize(tech : TechnologyName.T) : Int = if (tech == TechnologyName.Masonry) 3 else 0 /* stackSize here is taken from CivilizationFeatures */

  def buyCityWall(tech : TechnologyName.T) : Boolean = tech == TechnologyName.Masonry

  def speedLimit(tech : TechnologyName.T) : Int = if (tech == TechnologyName.HorsebackRiding) 3 else 0

}
