package civilization.helper

import civilization.objects._
import civilization.action.AbstractCommand
import civilization.gameboard.{GameBoard, MapTile, PlayerFigures}
import civilization.message.{M, Mess}


//Orientation.T

class RevealTileAction(override val param: Orientation.T) extends AbstractCommand(param) {

  private def generatePoints(b: GameBoard, p: P, o: Orientation.T): Seq[P] = {
    val ld: P = P(p.row * TILESIZE, p.col * TILESIZE)
    o match {
      case Orientation.Left =>
        (for (row <- 0 until TILESIZE) yield (P(ld.row + row, ld.col - 1))) toList
      case Orientation.Down =>
        (for (col <- 0 until TILESIZE) yield (P(ld.row - 1, ld.col + col))) toList
      case Orientation.Up =>
        (for (col <- 0 until TILESIZE) yield (P(ld.row + TILESIZE, ld.col + col))) toList
      case Orientation.Right =>
        (for (row <- 0 until TILESIZE) yield (P(ld.row + row, ld.col + TILESIZE))) toList
    }
  }

  private def revealTileVerify(b: GameBoard, civ: Civilization.T, p: P, o: Orientation.T): Mess = {
    val figo: Option[PlayerMove] = getCurrentMove(b, civ)
    if (figo.isEmpty) return Mess(M.CANNOTFINDSTARTOFMOVE, p)
    val fig: PlayerMove = figo.get
    val m: MapTile = getTile(b, p)
    if (m.orientation.isDefined) return (Mess(M.TILEALREADYREVEALED, p))
    if (!generatePoints(b, p, o).exists(_ == fig.lastp)) return (Mess(M.CANNOTREVEALFROMTHISPOINT, (fig.lastp, p)))
    null
  }

  def revealTileExecute(b: GameBoard, civ: Civilization.T, p: P, o: Orientation.T): Unit = revealTile(b, o, p)

  override def execute(board: GameBoard) = revealTileExecute(board, civ, p, param)

  override def verify(board: GameBoard): Mess = revealTileVerify(board, civ, p, param)
}
