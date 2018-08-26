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

trait TechnologyResourceTrait extends CommandPackage with ResourceActionTrait with ImplicitMiximFromJson with ImplicitMiximToJson {

  abstract protected class TechnologyResourceAction(override val param: HVResource) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      if (param.resource != resource(board)) return message.Mess(message.M.INCORRECTRESOURCEUSED, (command, techn, resource(board), param))
      if (Command.isTechnologyInCity(command)) {
        val cities: Seq[P] = itemizeH(board, deck)
        if (cities.find(_ == p).isEmpty) return message.Mess(message.M.RESOURCEALREADYUSEDORCITYAGAIN, (command, techn, resource(board), param))
      }
      null

    }

    def executeI(board: gameboard.GameBoard): Unit

    override def execute(board: gameboard.GameBoard): Unit = {
      decrResourceHVForTech(board, deck, resource(board), techn, isExecute)
      executeI(board)
    }
  }

  private def itemizeH(b: GameBoard, deck: PlayerDeck): Seq[P] = {
    if (!existResourceAndTech(b, deck, resource(b), techn)) return Nil
    if (Command.isTechnologyInCity(command))
    // cities available for city action
      CityAvailableForAction(b, deck.civ)
    else emptyItemizeP()
  }

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = itemizeH(b, deck)

}
