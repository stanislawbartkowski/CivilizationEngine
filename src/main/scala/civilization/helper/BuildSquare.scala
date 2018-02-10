package civilization.helper

import civilization.gameboard.{BuildingPoint, GameBoard}
import civilization.helper.BuyBuildingCommand.writesPoint
import civilization.message
import civilization.objects.{Civilization, P, S}
import play.api.libs.json.{JsValue, Json}

object BuildSquare {

  case class BuildSquare(val p: BuildingPoint, val remove: Seq[P])

  def toJB(p: BuildSquare): JsValue =
    Json.obj(
      S.p -> writesPoint(p.p.p),
      S.building -> p.p.b,
      S.list -> p.remove.map(writesPoint)
    )


  def toSJ(p: (P, Seq[BuildSquare])): JsValue = Json.obj(
    S.p -> writesPoint(p._1),
    S.list -> p._2.map(toJB(_))
  )

  def itemizeB(b: GameBoard, civ: Civilization.T, possibleP: (GameBoard, Civilization.T, P) => Seq[BuildSquare]): Seq[JsValue] = {
    val i: Seq[(P, Seq[BuildSquare])] =
      CityAvailableForAction(b, civ).map(city => (city, possibleP(b, civ, city)))
    // remove cities where nothing can be built
    i.filter(!_._2.isEmpty).map(toSJ(_))
  }

  def verifyB(board: GameBoard, civ: Civilization.T, p: P, param: BuildingPoint, m: message.M.Value, possibleP: (GameBoard, Civilization.T, P) => Seq[BuildSquare]): message.Mess = {
    val blds: Seq[BuildSquare] = possibleP(board, civ, p)
    val f: Option[BuildSquare] = blds.find(p => p == param)
    if (f.isDefined) return null
    message.Mess(m, param)
  }


}
