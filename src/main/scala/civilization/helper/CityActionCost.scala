package civilization.helper

import civilization.message.{FatalError, M, Mess}
import civilization.objects.Command

object CityActionCost {

  def actionCost(com: Command.T): Int =
    if (com == Command.BUYCITYWALL) 7
    else throw FatalError(Mess(M.IMPROPERACTIONTOCALCULATECOST, com))
}
