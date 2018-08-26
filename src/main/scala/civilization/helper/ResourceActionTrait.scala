package civilization.helper

import civilization.gameboard.GameBoard
import civilization.objects.{Command, Resource, TechnologyFeatures, TechnologyName}

trait ResourceActionTrait {

  val command: Command.T
  protected def techn : TechnologyName.T = TechnologyFeatures.commandResourceTechnology(command).get

  protected def resource(b: GameBoard): Resource.T = resourceForTech(b, techn)

  def getSet: Set[Command.T] = Set(command)


}
