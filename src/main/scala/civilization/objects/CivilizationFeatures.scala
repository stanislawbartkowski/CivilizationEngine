package civilization.objects

object CivilizationFeatures {

  def prodfortrade(civ : Civilization.T) : Int = if (civ == Civilization.America) 2 else DEFAULTPRODFORTRADE

  def freeGreatPersonAtTheBeginning(civ : Civilization.T) : Boolean = (civ == Civilization.America)

}
