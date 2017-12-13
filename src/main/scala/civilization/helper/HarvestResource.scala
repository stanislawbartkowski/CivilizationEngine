package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard.GameBoard
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, P, _}
import civilization.{gameboard, message}
import play.api.libs.json.JsValue

object HarvestResource extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.HARVESTRESOURCE)

  protected class HarvestResource(override val param: P) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess =
      defaultverify(board, civ, command, p, j)


    override def execute(board: gameboard.GameBoard): Unit = {
      val reso: Resource.T = getSquare(board, param).resource.get
      board.playerDeck(civ).resou.incr(reso)
      board.resources.resou.decr(reso)
    }
  }

  private def isResourceAvail(b: gameboard.GameBoard, s : MapSquareP): Boolean = s.resource.isDefined && b.resources.resou.nof(s.resource.get) > 0

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new HarvestResource(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = {
    // resources from scouts
    val li: Seq[(P, P)] = scoutsAvailableForAction(b, civ, (sc) => isResourceAvail(b, sc))
    // resources from outskirts
    val re : Seq[(P,P)] = CityAvailableForAction(b, civ).flatMap(s => squaresAround(b, s).filter(sc => isResourceAvail(b,sc)).map(pp => (s,pp.p)))
    (li ++ re).map(writesCityPoint)
  }

}