package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard._
import civilization.helper.BuildSquare._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir.GameResources
import civilization.io.tojson.ImplicitMiximToJson
import civilization.message
import civilization.objects.Command.T
import civilization.objects._
import play.api.libs.json.JsValue


object BuyBuildingCommand extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[T] = Set(Command.BUYBUILDING)

  private def findBuildings(b: GameBoard, city: P): Seq[MapSquareP] = squaresAround(b, city).filter(_.s.building.isDefined)

  private def findStarBuilding(b: GameBoard, city: P): Option[MapSquareP] = {
    val blds: Seq[MapSquareP] = findBuildings(b, city)
    val stars: Seq[MapSquareP] = blds.filter(_.s.building.get.isStar)
    // empty or one element
    if (stars.isEmpty) None else Some(stars.head)
  }

  private def listofunblocked(b: GameBoard, civ: Civilization.T, p: P): Seq[Building] = {
    val prod: ProdForCity = getProductionForCity(b, civ, p)
    val pl: PlayerDeck = b.playerDeck(civ)
    val alltechs: Seq[Technology] = pl.tech.map(t => GameResources.getTechnology(t.tech))
    // all unlocked buildings
    val allb: Seq[Building] = alltechs.filter(_.building.isDefined).map(b => GameResources.getBuilding(b.building.get)).
      // filter out unavailable, sold out
      filter(bld => b.market.buildings.noB(bld.name) > 0)

    // create set for quick lookup
    var setb: Set[BuildingName.T] = allb.map(_.name).toSet
    // remove upgraded already
    // remove buildings city cannot afford
    allb.filter(p =>
      (p.upgrade.isEmpty || !setb.contains(p.upgrade.get)) && (p.cost <= prod.prod)
    )
  }

  private def canbebuiltHere(m: MapSquareP, civ: Civilization.T, b: Building): Boolean = {
    if (!canBuild(m, civ)) return false
    if (b.terrain.isDefined) return b.terrain.get == m.sm.terrain
    m.sm.terrain != Terrain.Water
  }

  private def possibleBuildings(b: GameBoard, civ: Civilization.T, city: P): Seq[BuildSquare] = {
    val star: Option[MapSquareP] = findStarBuilding(b, city)
    val blist: Seq[Building] = listofunblocked(b, civ, city)
    // TODO : improve it
    val l: Seq[BuildSquare] = squaresAround(b, city).filter(s => s.civHere.isEmpty || s.civHere.get == civ).flatMap(p => blist.filter(canbebuiltHere(p, civ, _)).map(
      bui => {
        var po: Seq[P] = if (p.s.building.isDefined || p.s.wonder.isDefined) Seq(p.p) else Nil
        // throw off star building
        if (bui.star.isDefined && star.isDefined && p.p != star.get.p) po = po :+ star.get.p
        BuildSquare.BuildSquare(BuildingPoint(p.p, Some(bui.name), None), po)
      }
    ))
    // remove building on the same place
    l.filter(bui => {
      val s: MapSquareP = getSquare(b, bui.p.p)
      // filter out replacement the building by the same building
      s.s.building.isEmpty || s.s.building.get.name != bui.p.b
    })
  }

  protected class BuyBuilding(override val param: BuildingPoint) extends AbstractCommand(param) {

    override def verify(board: GameBoard): message.Mess = {
      //      val blds: Seq[BuildSquare] = possibleBuildings(board, civ, p)
      //      val f: Option[BuildSquare] = blds.find(p => p.p.p == param.p && p.p.b == param.b)
      //      if (f.isDefined) return null
      //      message.Mess(message.M.CANNOTBUYBUILDINGHERE, param)
      verifyB(board, civ, p, param, message.M.CANNOTBUYBUILDINGHERE, possibleBuildings)
    }

    override def execute(board: GameBoard): Unit = {
      val bui: Building = GameResources.getBuilding(param.b)
      if (bui.isStar) {
        // remove star building if exists
        val sta: Option[MapSquareP] = findStarBuilding(board, p)
        if (sta.isDefined) removeBuilding(board, sta.get)
      }
      val ma: MapSquareP = getSquare(board, param.p)
      // remove existing if exists
      if (ma.s.building.isDefined)
        removeBuilding(board, ma)
      // remove wonder if exist
      if (ma.s.wonder.isDefined)
        removeWonder(board, ma)
      // withdraw from market
      board.market.buildings.incdevBuilding(param.b, false)
      // built
      ma.s.setBuilding(param.b)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new BuyBuilding(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = {
    //    val i: Seq[(P, Seq[BuildSquare])] =
    //      CityAvailableForAction(b, civ).map(city => (city, possibleBuildings(b, civ, city)))
    // remove cities where nothing can be built
    //    i.filter(!_._2.isEmpty).map(toSJ(_))
    itemizeB(b, civ, possibleBuildings)
  }

}
