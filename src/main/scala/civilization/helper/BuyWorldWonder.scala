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

  override def getSet: Set[Command.T] = Set(Command.BUYWONDER, Command.RANDOMWONDER, Command.FREEWONDER);

  private def canAffordWonder(b: GameBoard, pl: PlayerDeck, prod: ProdForCity, w: WondersOfTheWorld): Boolean = {
    if (prod.prod >= w.cost) return true
    // check discount
    if (w.discount.isEmpty) return false
    // player has technology ?
    //    if (pl.tech.find(_.tech == w.discount.get.tech).isEmpty) return false
    if (!pl.hasTechnology(w.discount.get.tech)) return false
    // check against discount
    return prod.prod >= w.discount.get.cost
  }

  private def wondersForCity(b: GameBoard, pl: PlayerDeck, city: P): Seq[Wonders.T] = {

    // tricky
    if (pl.freeWonder.isDefined) return Seq(pl.freeWonder.get)

    // production for city
    val prod: ProdForCity = getProductionForCity(b, pl, city)
    // wonders available
    val wond: Seq[WondersOfTheWorld] = b.getCurrentWonders().map(GameResources.getWonder(_))
    // filter out wonders too expensive
    wond.filter(canAffordWonder(b, pl, prod, _)).map(_.name)
  }

  private def findBuiltWonder(points: Seq[MapSquareP]): Option[P] = {
    val m: Option[MapSquareP] = points.find(_.s.wonder.isDefined)
    if (m.isDefined) Some(m.get.p) else None
  }

  private def possibleWonders(b: GameBoard, deck: PlayerDeck, city: P): Seq[BuildSquare] = {
    var res: Seq[BuildSquare] = Nil
    // possible squares, all except Water
    var points: Seq[MapSquareP] = getOutskirtsForBuild(b, deck.civ, city)
    val wond: Option[P] = findBuiltWonder(points)
    // TODO: improve,
    wondersForCity(b, deck, city).foreach(w => {
      res = res ++ points.map(p => {
        // remove existing wonder
        val r: Seq[P] = if (wond.isDefined && wond.get != p.p) Seq(wond.get) else Nil
        val remove: Seq[P] = r ++ getStructureHere(p)

        BuildSquare(BuildingPoint(p.p, None, Some(w), None), remove)
      })
    })
    res
  }

  private def removeWonderFromMarket(b: GameBoard, w: Wonders.T) = b.market.wonders = b.market.wonders.filter(_ != w)

  protected class BuyWonder(override val param: BuildingPoint) extends AbstractCommand(param) {


    override def registerCommandInJournal(board: GameBoard) = registerCommandInJournalDefault(board, JournalElem.constructJA(param.w))

    override def verify(board: gameboard.GameBoard): message.Mess =
      verifyB(board, deck, p, param, message.M.CANNOTBUYWONDERGHERE, possibleWonders)

    override def execute(board: gameboard.GameBoard): Unit = {
      val wond: Option[P] = findBuiltWonder(outskirtsForCityNotBlocked(board, civ, p))
      if (wond.isDefined) {
        val w: MapSquareP = getSquare(board, wond.get)
        // remove existing wonder
        removeWonder(board, w)
      }
      val ww: MapSquareP = getSquare(board, param.p)
      // destroy building if exist
      //      if (ww.s.building.isDefined) removeBuilding(board, ww)
      removeStructure(board, ww)
      // build wonder
      ww.s.wonder = Some(WonderSquare(param.w, false))
      // remove wonder from list
      removeWonderFromMarket(board, param.w)
      // remove free
      deck.freeWonder = None
      if (command == Command.BUYWONDER) advanceCultureForFree(board, civ, isExecute)
    }
  }

  protected class RandomWonder(override val param: Wonders.T) extends AbstractCommand(param) {
    override def verify(board: GameBoard): message.Mess = null

    override def execute(board: GameBoard): Unit = deck.freeWonder = Some(param)
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue) =
    if (command == Command.BUYWONDER) new BuyWonder(param)
    else if (command == Command.RANDOMWONDER) new RandomWonder(param)
    else new BuyWonder(param)

  override def itemize(b: GameBoard, deck: PlayerDeck, com: Command.T): Seq[JsValue] = {
    if (com == Command.FREEWONDER) if (deck.freeWonder.isEmpty) return Nil
    itemizeB(b, deck, false, possibleWonders)
  }
}
