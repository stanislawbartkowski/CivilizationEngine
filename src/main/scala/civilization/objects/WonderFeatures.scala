package civilization.objects

object WonderFeatures {

  def combatBonus(won : Wonders.T) : Int = if (won == Wonders.StatueofZeus) 6 else 0

}
