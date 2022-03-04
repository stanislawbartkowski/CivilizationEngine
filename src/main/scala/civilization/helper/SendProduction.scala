package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.helper.SpendTrade.{SpendTrade, toInt}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}
import play.api.libs.json.JsValue


object SendProduction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.SENDPRODUCTION, Command.UNDOSENDPRODUCTION)


  private def itemizeCommandsForSendProduction(b: gameboard.GameBoard, civ: Civilization.T): Seq[(P, P)] =
    scoutsAvailableForAction(b,civ,(sc) => sc.numberOfProduction > 0)

    // (city,scout)
  private def itemizeCommandsForUndoSendProduction(b: gameboard.GameBoard, civ: Civilization.T): Seq[(P, P)] = {
    // scout => city
    val ma: Map[P, P] = sendprodForScouts(b, civ)
    val c : Seq[P] = CityAvailableForAction(b,civ)
    // only cities eligible for action
    ma.filter(pp => c.find(_ == pp._2).isDefined).map(pp => (pp._2,pp._1)) toSeq
  }

  protected class SendProduction(override val param: P) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val res : Option[Mess]= verifyScoutForAction(board,civ,param)
      if (res.isDefined) return res.get
      val sc: Map[P, P] = sendprodForScouts(board, civ)
      if (sc.get(param).isDefined)
        return Mess(M.CANNOTSENTPRODTWICESCOUT, (param))
      null
    }

    override def execute(board: gameboard.GameBoard): Unit = ()
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) =
    if (command == Command.SENDPRODUCTION) new SendProduction (param) else emptyCommandPoint(param)

  override def itemize(b: GameBoard, deck : PlayerDeck, com: Command.T): Seq[JsValue] = {
    val li : Seq[(P, P)] = if (com == Command.SENDPRODUCTION) itemizeCommandsForSendProduction(b,deck.civ) else itemizeCommandsForUndoSendProduction(b,deck.civ)
    li.map(writesCityPoint)
  }

}
