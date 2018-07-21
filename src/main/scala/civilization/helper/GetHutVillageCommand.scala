package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.gameboard._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects._
import play.api.libs.json.JsValue

object GetHutVillageCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.GETHUTVILLAGE, Command.DROPHUTVILLAGE)

  protected class GetHutVillage(override val param: HutVillage) extends AbstractCommand(param) {

    override protected def verify(board: GameBoard): Mess = null

    override protected def execute(board: GameBoard): Unit = {
      command match {
        case Command.DROPHUTVILLAGE => {
          val fun: (HutVillage, HutVillage) => Boolean = (p1: HutVillage, p2: HutVillage) => {
            p1 == p2
          }
          deck.hvlist = removeElem(deck.hvlist, param, fun)
        }
        case Command.GETHUTVILLAGE =>  deck.hvlist = deck.hvlist :+ param
      }
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command = new GetHutVillage(param)

}

