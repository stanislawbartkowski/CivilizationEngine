package civilization.helper

import civilization.action.AbstractCommand
import civilization.message.{M, Mess}
import civilization.objects._
import civilization.{gameboard, message}


object SendProduction {

  private def allScouts(b: gameboard.GameBoard, civ: Civilization.T) : Seq[MapSquareP] = getFigures(b, civ).filter(_.s.figures.numberofScouts > 0)

  private def allOutSkirts(b: gameboard.GameBoard, civ: Civilization.T): Seq[MapSquareP] =
    citiesForCivilization(b, civ).map(p => squaresAround(b, p.p)).flatten

  // (city, scout)
  def itemizeCommandsForSendProduction(b: gameboard.GameBoard, civ: Civilization.T): Seq[(P, P)] = {
    val out: Seq[MapSquareP] = allOutSkirts(b, civ)
    // filter out scout on outskirts
    // filter also all on squares without production
    val fig: Seq[MapSquareP] = allScouts(b,civ).filter(sc => out.find(_.p == sc.p).isEmpty).filter(sc => sc.numberOfProduction > 0)
    val c : Seq[P] = CityAvailableForAction(b,civ)
    val scoutsused : Map[P,P] = sendprodForScouts(b,civ)
    // filter out all scouts used already
    val scouts : Seq[P] = fig.filter(sc => scoutsused.get(sc.p).isEmpty).map(_.p)
    // all cities x all scouts, cartesian product
    c.map(cit => scouts.map(sc => (cit,sc))).flatten
  }

  // (city,scout)
  def itemizeCommandsForUndoSendProduction(b: gameboard.GameBoard, civ: Civilization.T): Seq[(P, P)] = {
    // scout => city
    val ma: Map[P, P] = sendprodForScouts(b, civ)
    val c : Seq[P] = CityAvailableForAction(b,civ)
    // only cities eligible for action
    ma.filter(pp => c.find(_ == pp._2).isDefined).map(pp => (pp._2,pp._1)) toSeq
  }

  class SendProduction(override val param: P) extends AbstractCommand(param) {

    override def verify(board: gameboard.GameBoard): message.Mess = {
      val fig: Seq[MapSquareP] = allScouts(board,civ)
      val scout: P = param.asInstanceOf[P]
      if (fig.find(_.p == param).isEmpty) return Mess(M.NOSCOUTATTHISPOINT, (scout))
      val out: Seq[MapSquareP] = allOutSkirts(board, civ)
      if (out.find(_.p == scout).isDefined)
        return Mess(M.SCOUTISONCITYOUTSKIRT, (scout))
      val sc: Map[P, P] = sendprodForScouts(board, civ)
      if (sc.get(scout).isDefined)
        return Mess(M.CANNOTSENTPRODTWICESCOUT, (scout))
      null
    }

    override def execute(board: gameboard.GameBoard): Unit = Unit
  }


}
