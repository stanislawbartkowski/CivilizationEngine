package civilization.objects

import civilization.message._

object ObjectCost {

  def getCost(o : AnyRef) : Int = {
    o match {
      case f : Figure.T => if (f == Figure.Scout) 6 else 4
      case _ =>  throw FatalError(Mess(M.CANNOTGIVECOSTFORTHISOBJECT,o))
    }
  }

}
