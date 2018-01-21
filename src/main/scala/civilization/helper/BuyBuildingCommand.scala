package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message
import civilization.objects.Command.T
import civilization.objects._
import play.api.libs.json.JsValue
import civilization.io.readdir.GameResources
import play.api.libs.json.{JsValue, Json}


object BuyBuildingCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  private case class BuildSquare(val p: BuildingPoint, val remove: Seq[P])

  def toJB(p: BuildSquare): JsValue =
    Json.obj(
      S.p -> writesPoint(p.p.p),
      S.building -> p.p.b,
      S.list -> p.remove.map(writesPoint)
    )


  private def toSJ(p: (P, Seq[BuildSquare])): JsValue = Json.obj(
    S.p -> writesPoint(p._1),
    S.list -> p._2.map(toJB(_))
  )

  override def getSet: Set[T] = Set(Command.BUYBUILDING)

  private def findBuildings(b: GameBoard, city: P): Seq[MapSquareP] = squaresAround(b, city).filter(_.s.building.isDefined)

  private def findStarBuilding(b: GameBoard, city: P): Option[MapSquareP] = {
    val blds : Seq[MapSquareP] = findBuildings(b,city)
    val stars : Seq[MapSquareP] = blds.map(s => (s, GameResources.getBuilding(s.s.building.get))).filter(s => s._2.star.isDefined && s._2.star.get).map(_._1)
    // empty or one element
    if (stars.isEmpty) None else Some(stars.head)
  }

  private def listofunblocked(b: GameBoard, civ: Civilization.T, p: P): Seq[Building] = {
    val prod: ProdForCity = getProductionForCity(b, civ, p)
    val pl: PlayerDeck = b.playerDeck(civ)
    val alltechs: Seq[Technology] = pl.tech.map(t => GameResources.getTechnology(t.tech))
    // all unlocked buildings
    val allb: Seq[Building] = alltechs.filter(_.building.isDefined).map(b => GameResources.getBuilding(b.building.get))
    // create set for quick lookup
    var setb: Set[BuildingName.T] = allb.map(_.name).toSet
    // remove upgraded already
    // remove buildings city cannot afford
    allb.filter(p =>
      (p.upgrade.isEmpty || !setb.contains(p.upgrade.get)) && (p.cost <= prod.prod)
    )
  }

  private def canbebuiltHere(m: MapSquareP, b: Building): Boolean = {
    if (b.terrain.isDefined) return b.terrain.get == m.sm.terrain
    m.sm.terrain != Terrain.Water
  }

  private def possibleBuildings(b: GameBoard, civ: Civilization.T, city: P): Seq[BuildSquare] = {
    val star: Option[MapSquareP] = findStarBuilding(b, city)
    val blist: Seq[Building] = listofunblocked(b, civ, city)
    squaresAround(b, city).filter(s => s.civHere.isEmpty || s.civHere.get == civ).flatMap(p => blist.filter(canbebuiltHere(p, _)).map(
      bui => {
        var po: Seq[P] = Nil
        // replace
        if (p.s.building.isDefined) po = Seq(p.p)
        // throw off star building
        if (bui.star.isDefined && star.isDefined && p.p != star.get.p) po = po :+ p.p
        BuildSquare(BuildingPoint(p.p, bui.name), po)
      }
    ))
  }

  protected class BuyBuilding(override val param: BuildingPoint) extends AbstractCommand(param) {
    override def verify(board: GameBoard): message.Mess = {
       val blds : Seq[BuildSquare] = possibleBuildings(board,civ,p)
       val f : Option[BuildSquare] = blds.find(p => p.p.p == param.p && p.p.b == param.b)
       if (f.isDefined) return null
       message.Mess(message.M.CANNOTBUYBUILDINGHERE,param)
    }

    override def execute(board: GameBoard): Unit = {
      val bui: Building = GameResources.getBuilding(param.b)
      if (bui.star.isDefined && bui.star.get) {
        // remove star building if exists
        val sta: Option[MapSquareP] = findStarBuilding(board, param.p)
        if (sta.isDefined) sta.get.s.building = None
      }
      val ma: MapSquareP = getSquare(board, param.p)
      ma.s.building = Some(param.b)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new BuyBuilding(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = {
    val i: Seq[(P, Seq[BuildSquare])] =
      CityAvailableForAction(b, civ).map(city => (city, possibleBuildings(b, civ, city)))
    // remove cities where nothing can be built
    i.filter(!_._2.isEmpty).map(toSJ(_))
  }

}
