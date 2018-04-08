package civilization.helper

import civilization.action.{AbstractCommand, CommandPackage}
import civilization.gameboard.{BuildingPoint, _}
import civilization.helper.BuildSquare._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, P, _}
import civilization.{gameboard, message}
import play.api.libs.json.JsValue
object BuyWorldWonder extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {


  private def canAffordWonder(b: GameBoard, pl: PlayerDeck, prod: ProdForCity, w: WondersOfTheWorld): Boolean = {
    if (prod.prod >= w.cost) return true
    // check discount
    if (w.discount.isEmpty) return false
    // player has technology ?
    if (pl.tech.find(_.tech == w.discount.get.tech).isEmpty) return false
    // check against discount
    return prod.prod >= w.discount.get.cost
  }

  private def wondersForCity(b: GameBoard, civ: Civilization.T, city: P): Seq[Wonders.T] = {
    // production for city
    val prod: ProdForCity = getProductionForCity(b, civ, city)
    // wonders available
    val wond: Seq[WondersOfTheWorld] = b.getCurrentWonders().map(GameResources.getWonder(_))
    // player deck
    val pl: PlayerDeck = b.playerDeck(civ)
    // filter out wonders too expensive
    wond.filter(canAffordWonder(b, pl, prod, _)).map(_.name)
  }

  private def findBuiltWonder(points: Seq[MapSquareP]): Option[P] = {
    val m: Option[MapSquareP] = points.find(_.s.wonder.isDefined)
    if (m.isDefined) Some(m.get.p) else None
  }

  private def getablePoints(b: GameBoard, civ: Civilization.T, city: P): Seq[MapSquareP] = squaresAround(b, city).filter(ss => canBuild(ss, civ) && ss.sm.terrain != Terrain.Water)

  private def possibleWonders(b: GameBoard, civ: Civilization.T, city: P): Seq[BuildSquare] = {
    var res: Seq[BuildSquare] = Nil
    // possible squares, all except Water
    var points: Seq[MapSquareP] = getablePoints(b, civ, city)
    val wond: Option[P] = findBuiltWonder(points)
    // TODO: improve,
    wondersForCity(b, civ, city).foreach(w => {
      res = res ++ points.map(p => {
        // remove existing wonder
        var remove: Seq[P] = if (wond.isDefined) Seq(wond.get) else Nil
        // remove also building if exists
        if (p.s.building.isDefined) remove = remove :+ p.p
        BuildSquare.BuildSquare(BuildingPoint(p.p, None, Some(w)), remove)
      })
    })
    res
  }

  private def removeWonderFromMarket(b : GameBoard, w : Wonders.T) = b.market.wonders = b.market.wonders.filter(_ != w)

  override def getSet: Set[Command.T] = Set(Command.BUYWONDER);

  protected class BuyWonder(override val param: BuildingPoint) extends AbstractCommand(param) {
    override def verify(board: gameboard.GameBoard): message.Mess =
      verifyB(board, civ, p, param, message.M.CANNOTBUYWONDERGHERE, possibleWonders)

    override def execute(board: gameboard.GameBoard): Unit = {
      val wond: Option[P] = findBuiltWonder(getablePoints(board, civ, p))
      if (wond.isDefined) {
        val w: MapSquareP = getSquare(board, wond.get)
        // remove existing wonder
        removeWonder(board, w)
      }
      val ww: MapSquareP = getSquare(board, param.p)
      // destroy building if exist
      if (ww.s.building.isDefined) removeBuilding(board, ww)
      // build wonder
      ww.s.wonder = Some(WonderSquare(param.w, false))
      // remove wonder from list
      removeWonderFromMarket(board,param.w)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) = new BuyWonder(param)

  override def itemize(b: GameBoard, civ: Civilization.T, com: Command.T): Seq[JsValue] =
    itemizeB(b, civ, possibleWonders)
}
