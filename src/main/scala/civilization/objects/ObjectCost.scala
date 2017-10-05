package civilization.objects

import civilization.message._

object ObjectCost {

  def getCost(o: AnyRef, strength: Int): Int = o match {
    case f: Figure.T => if (f == Figure.Scout) 6 else 4
    case c : CombatUnitType.T =>
      if (c == CombatUnitType.Aircraft) 12
      else 5 + strength*2
    case _ => throw FatalError(Mess(M.CANNOTGIVECOSTFORTHISOBJECT, o))
  }

  def getCost(o: AnyRef): Int = getCost(o, -1)

}
