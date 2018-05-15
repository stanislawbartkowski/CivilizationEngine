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

  private def canbebuiltHere(m: MapSquareP, b: Building): Boolean = {
      if (b.terrain.isEmpty) true else b.terrain.get == m.sm.terrain
  }

  private def possibleBuildings(b: GameBoard, civ: Civilization.T, city: P): Seq[BuildSquare] = {
    val star: Option[MapSquareP] = findStarBuilding(b, city)
    val blist: Seq[Building] = listofunblocked(b, civ, city)
    // TODO : improve it
    var points: Seq[MapSquareP] = outskirtsForCityNotBlocked(b, civ, city)
    val l: Seq[BuildSquare] = points.flatMap(p => blist.filter(canbebuiltHere(p, _)).map(
      bui => {
//        var po: Seq[P] = if (p.s.building.isDefined || p.s.wonder.isDefined || p.s.greatperson.isDefined) Seq(p.p) else Nil
        var po : Seq[P] = getStructureHere(p)
        // throw off star building
        if (bui.star.isDefined && star.isDefined && p.p != star.get.p) po = po :+ star.get.p
        BuildSquare.BuildSquare(BuildingPoint(p.p, Some(bui.name), None,None), po)
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
//      if (ma.s.building.isDefined)
//        removeBuilding(board, ma)
      // remove wonder if exist
//      if (ma.s.wonder.isDefined)
//        removeWonder(board, ma)
      removeStructure(board,ma)
      // withdraw from market
      board.market.buildings.incdevBuilding(param.b, false)
      // built
      ma.s.setBuilding(param.b)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new BuyBuilding(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] = {
    itemizeB(b, civ, false, possibleBuildings)
  }

}
