package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.helper.CurrencyAction.CurrencyAction
import civilization.helper.DemocracyAction.emptyItemize
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.{gameboard, message}
import civilization.objects.{Command, HVResource}
import civilization.objects._
import play.api.libs.json.JsValue

trait TechnologyResourceTrait extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  val command: Command.T
  val tech: TechnologyName.T

  protected def resource(b: GameBoard): Resource.T = resourceForTech(b, tech)

  override def getSet: Set[Command.T] = Set(command)

  abstract protected class TechnologyResourceAction(override val param: HVResource) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      if (param.resource != resource(board)) return message.Mess(message.M.INCORRECTRESOURCEUSED, (command, tech, resource(board), param))
      if (Command.isTechnologyInCity(command)) {
        val cities: Seq[P] = itemizeH(board, deck)
        if (cities.find(_ == p).isEmpty) return message.Mess(message.M.RESOURCEALREADYUSEDORCITYAGAIN, (command, tech, resource(board), param))
      }
      null

    }

    def executeI(board: gameboard.GameBoard): Unit

    override def execute(board: gameboard.GameBoard): Unit = {
      decrResourceHVForTech(board, deck, resource(board), tech, isExecute)
      executeI(board)
    }
  }

  private def itemizeH(b: GameBoard, deck: PlayerDeck): Seq[P] = {
    if (!existResourceAndTech(b, deck, resource(b), tech)) return Nil
    if (Command.isTechnologyInCity(command))
    // cities available for city action
      CityAvailableForAction(b, deck.civ)
    else emptyItemizeP()
  }

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = itemizeH(b, deck)

}
