package civilization.helper

import civilization.objects.{Command, TechnologyFeatures, TechnologyName}

trait ResourceActionTrait {

  val command: Command.T
  def techn : TechnologyName.T = TechnologyFeatures.commandResourceTechnology(command).get

  def getSet: Set[Command.T] = Set(command)


}
