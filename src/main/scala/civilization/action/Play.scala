package civilization.action

object Play {

  class Play {
    val commands: collection.mutable.ListBuffer[Command] = collection.mutable.ListBuffer()
    def addCommand(com : Command) = commands += com
  }

}
