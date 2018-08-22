package civilization.II.interfaces

import civilization.gameboard.{GameBoard}

trait ICache {

  def getT(id : Int, retrieve : (Int) => GameBoard) : GameBoard

  def isCached : Boolean

}
