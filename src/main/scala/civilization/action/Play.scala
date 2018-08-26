package civilization.action

object Play {

  class Play {
    val commands: collection.mutable.ListBuffer[Command] = collection.mutable.ListBuffer()

    def addCommand(com: Command) = commands += com

    def getLastSuspended: (Int, Option[Command]) =
      if (commands.isEmpty) (-1, None) else {
        var counter: Int = commands.length - 1
        var l: Option[Command] = None
        // iterate to get index
        while (counter >= 0 && !commands(counter).isSuspended) counter = counter - 1
        (counter, Some(commands(counter)))
      }
  }

}
