package civilization.test

import civilization.I
import civilization.action.{Command, constructCommand}
import org.scalatest.FunSuite
import civilization.gameboard._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.objects._
import civilization.helper._
import civilization.io.fromjson.toJ
import play.api.libs.json.JsValue
import Helper.II


class Test2 extends FunSuite {

  Helper.I

  test("Test board") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME1.json")
    val li: Seq[P] = allPoints(b)
    li.foreach(println)
    assert(32 == li.length)
    assert(isPointOnBoard(b, P(0, 0)))
    assert(isPointOnBoard(b, P(7, 3)))
    assert(!isPointOnBoard(b, P(7, 4)))
    val po: Seq[P] = pointsAround(b, P(0, 0))
    println("Points around 0,0")
    po.foreach(println)
    assert(3 == po.length)
    assert(po.exists(_ == P(1, 1)))
    assert(!po.exists(_ == P(1, 2)))
    val po1: Seq[P] = pointsAround(b, P(1, 1))
    po1.foreach(println)
    assert(8 == po1.length)
    val po2: Seq[P] = pointsAround(b, P(7, 3))
    po2.foreach(println)
    assert(3 == po2.length)
  }

  private def eqr(r : Resource.T,sq1: Square) = sq1.resource.isDefined && sq1.resource.get == r

  test("Test board content") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME1.json")
    val m: MapSquareP = getSquare(b, P(0, 0))
    println(m)
    assert(!m.revealed)
    assert(eqr(Resource.Coin,m.sm))
    val m1: MapSquareP = getSquare(b, P(4, 0))
    println("4:0 -----------------------")
    println(m1)
    assert(m1.sm.hv == HutVillage.Village)
  }

  test("Beginning Deploy Scout and Army") {
    val reg = Helper.readBoardAndPlayT("test2/BOARDGAME3.json", "test2/GAME1.json", Civilization.Germany)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.ENDOFPHASE).isEmpty)
    var js: String = "{\"row\":1, \"col\" : 1}"
    Helper.executeCommandH(token, "SETSCOUT", 2, 2, js)
    g = I.getBoardForToken(token)
    l = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.ENDOFPHASE).isDefined)
    assert(l.find(_ == Command.SETSCOUT).isEmpty)
    assert(l.find(_ == Command.SETARMY).isEmpty)
  }
}