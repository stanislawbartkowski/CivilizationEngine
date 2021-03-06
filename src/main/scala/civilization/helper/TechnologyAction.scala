package civilization.helper

import civilization.gameboard._
import civilization.objects._
import civilization.message.J


object TechnologyAction {

  def BattleWinner(b: GameBoard, pl: PlayerDeck, isExecute: Boolean) = {
    // look for CodeOfLaw benefir
    pl.tech.foreach(te => {
      if (te.tech == TechnologyName.CodeOfLaw) {
        if (te.coins < COINSCAPACITY) // increase
          if (TechnologyUsedAlready(b, te)) addToJournal(b, pl, isExecute, J.COINADDEDALREDYINTHISTURN, Nil, JournalElem.constructJA(te.tech))
          else {
            addCoinToTechnology(b, pl, te, isExecute)
            addToJournal(b, pl, isExecute, J.PUTCOINAFTERBATTLE, Nil, JournalElem.constructJA(te.tech))
          }
        else addToJournal(b, pl, isExecute, J.CANNOTADDCOINLIMIT, Seq("" + COINSCAPACITY), JournalElem.constructJA(te.tech))
      }

    })
  }

}
