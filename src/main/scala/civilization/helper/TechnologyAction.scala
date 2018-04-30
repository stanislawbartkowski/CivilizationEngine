package civilization.helper

import civilization.gameboard._
import civilization.objects._
import civilization.message.J


object TechnologyAction {

  def BattleWinner(b: GameBoard, winner: Civilization.T) = {
    // look for CodeOfLaw benefir
    val pl: PlayerDeck = b.playerDeck(winner)
    pl.tech.foreach(te => {
      if (te.tech == TechnologyName.CodeOfLaw) {
        if (te.coins < COINSCAPACITY) // increase
          if (TechnologyUsedAlready(b, te)) addToJournal(b, winner, J.COINADDEDALREDYINTHISTURN, null, Some(te.tech))
          else {
            addCoinToTechnology(b, te)
            addToJournal(b, winner, J.PUTCOINAFTERBATTLE, null, Some(te.tech))
          }
        else addToJournal(b, winner, J.CANNOTADDCOINLIMIT, Seq(COINSCAPACITY), Some(te.tech))
      }

    })
  }

}
