package civilization.helper

import civilization.action._
import civilization.gameboard.{GameBoard, PlayerDeck}
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message.{M, Mess}
import civilization.objects.Command.T
import civilization.objects.{Civilization, Command, P, _}
import civilization.{gameboard, message}
import play.api.libs.json.JsValue

object BuildCityWalls extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[T] = Set(Command.BUILDCITYWALLFORFREE, Command.BUYCITYWALL)

  protected class BuildCityWalls extends AbstractCommandNone {
    override def verify(board: gameboard.GameBoard): message.Mess = {
      val city: MapSquareP = getSquare(board, p)
      if (City.isWalled(city.s.city.get.citytype)) return new Mess(M.CITYALREADYWALLED, (command, civ, p))
      return null
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      val s : MapSquareP = getSquare(board, p)
      // build wall
      s.s.city = Some(City(s.civHere.get,City.toWalled(s.s.city.get.citytype)))
    }

  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new BuildCityWalls

  override def itemizePP(b: GameBoard, deck : PlayerDeck, com: Command.T): Seq[P] =
    if (com == Command.BUILDCITYWALLFORFREE) Nil
    else if (!deck.hasTechnologyFeature(TechnologyFeatures.buyCityWall)) Nil
    else CitiesCanAfford(b, deck, com).filter(c => !City.isWalled(getSquare(b,c).s.city.get.citytype))
}
