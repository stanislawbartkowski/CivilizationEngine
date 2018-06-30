package civilization.helper

import civilization.gameboard._
import civilization.objects._
import civilization.message.J


object TechnologyAction {

  def BattleWinner(b: GameBoard, pl: PlayerDeck) = {
    // look for CodeOfLaw benefir
    pl.tech.foreach(te => {
      if (te.tech == TechnologyName.CodeOfLaw) {
        if (te.coins < COINSCAPACITY) // increase
          if (TechnologyUsedAlready(b, te)) addToJournal(b, pl, J.COINADDEDALREDYINTHISTURN, null, Some(te.tech))
          else {
            addCoinToTechnology(b, te)
            addToJournal(b, pl, J.PUTCOINAFTERBATTLE, null, Some(te.tech))
          }
        else addToJournal(b, pl, J.CANNOTADDCOINLIMIT, Seq(COINSCAPACITY), Some(te.tech))
      }

    })
  }

}
