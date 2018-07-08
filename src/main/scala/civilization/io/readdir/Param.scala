package civilization.io.readdir

import civilization.objects.{Civilization, HVResource}

object Param {

  case class HVResourcesForCiv(resource: Seq[HVResource], civ: Option[Civilization.T])

  case class HVResourceCiv(resource: HVResource, civ: Option[Civilization.T])

}
