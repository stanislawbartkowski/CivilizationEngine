package civilization.helper

import civilization.action.{AbstractCommand, Command, CommandPackage}
import civilization.{gameboard, message}
import civilization.gameboard.GameBoard
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects._
import play.api.libs.json.JsValue
import civilization.gameboard._
import civilization.helper.BuildSquare._
import civilization.helper.BuyWorldWonder.possibleWonders


object GreatPersonAction extends CommandPackage with ImplicitMiximFromJson with ImplicitMiximToJson {

  override def getSet: Set[Command.T] = Set(Command.GREATPERSONPUTNOW, Command.GREATPERSONPUTNOWRESIGN, Command.GREATPERSONPUT)

  private def possibleGreatPersons(gp: Seq[GreatPersonName.T])(b: GameBoard, deck: PlayerDeck, city: P): Seq[BuildSquare] = {
    getOutskirtsForBuild(b, deck.civ, city).flatMap(pp => gp.map(g =>
      BuildSquare(BuildingPoint(pp.p, None, None, Some(g)), getStructureHere(pp))
    ))
  }

  protected class GreatPersonOnBoard(override val param: BuildingPoint) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      def f: PossibleP = possibleGreatPersons(Seq(param.g))
      verifyB(board, deck, p, param, message.M.CANNOTPUTGREATPERSONHERE, f)
    }

    override def execute(board: gameboard.GameBoard): Unit = {
      val ma: MapSquareP = getSquare(board, param.p)
      removeStructure(board,ma)
      // put great person
      ma.s.setGreatPerson(param.gp.get)
    }
  }

  override def produceCommand(command: Command.T, civ: Civilization.T, p: P, param: JsValue): Command =
    if (command == Command.GREATPERSONPUTNOWRESIGN) emptyCommand() // place holder only
    else new GreatPersonOnBoard(param)


  override def itemize(b: GameBoard, deck : PlayerDeck, com: Command.T): Seq[JsValue] = {
    var l : Seq[GreatPersonName.T] = Nil
    if (com != Command.GREATPERSONPUT) {
      val gp: Option[GreatPersonName.T] = isGreatPersonNow(b, deck.civ)
      if (gp.isEmpty) return Nil
      l = Seq(gp.get)
      if (com == Command.GREATPERSONPUTNOWRESIGN) return l
    }
    else l = greatPersonReady(b,deck)
    // curry function
    def f: PossibleP = possibleGreatPersons(l)
    itemizeB(b, deck, true, f)
  }

}
