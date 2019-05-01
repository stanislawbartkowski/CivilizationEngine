package civilization.objects

object CivilizationFeatures {

  def tradeForProd(civ : Civilization.T) : Int = if (civ == Civilization.America) 2 else DEFAULTTRADEFORPROD

  def freeGreatPersonAtTheBeginning(civ : Civilization.T) : Boolean = civ == Civilization.America

  def buildWallInCapital(civ:Civilization.T) : Boolean = civ == Civilization.China

  def get3CultureForHutOrVillage(civ : Civilization.T) : Boolean = civ == Civilization.China

  def canSaveUnit(civ : Civilization.T) : Boolean = civ == Civilization.China

  def freeWonderOfTheWorldAtTheBeginning(civ : Civilization.T) : Boolean = civ == Civilization.Egypt

  def freeUnlockedBuildingCityManagement(civ : Civilization.T) : Boolean = civ == Civilization.Egypt

  def takefree2Infantry(civ : Civilization.T) : Boolean = civ == Civilization.Germany

  def takefreeResourceAfterUpgradingMilitary(civ : Civilization.T) : Boolean = civ == Civilization.Germany

  def advanceCultureWonderCityVillage(civ : Civilization.T) : Boolean = civ == Civilization.Rome

  def startStackingLimit(civ : Civilization.T) : Int = if (civ == Civilization.Russia) 3 else 2

  def numberofArmiesToStart(civ : Civilization.T) : Int = if (civ == Civilization.Russia) 2 else 1

  def numberofScoutsToStart(civ : Civilization.T) : Int = if (civ == Civilization.Spain) 2 else 1

  def canSacrificeFigureForTech(civ : Civilization.T) : Boolean = civ == Civilization.Russia

  def increaseTravelSpeedByOne(civ : Civilization.T) : Boolean = civ == Civilization.Spain

  def freeBuilidingAfterRevealTile(civ : Civilization.T) : Boolean = civ == Civilization.Spain

  def freeResourcesAtStart(civ : Civilization.T) : Boolean = civ == Civilization.Arabs

  def freeCultureForResourceSpend(civ : Civilization.T) : Boolean = civ == Civilization.Arabs

}
