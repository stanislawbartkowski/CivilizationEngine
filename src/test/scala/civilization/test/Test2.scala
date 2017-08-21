package civilization.test

import org.scalatest.FunSuite
import civilization.gameboard._
import civilization.objects._
import civilization.helper._

class Test2 extends FunSuite {

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

  test("Test board content") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME1.json")
    val m: MapSquareP = getSquare(b, P(0, 0))
    println(m)
    assert(!m.revealed)
    assert(Resource.Coin == m.sm.resource)
    val m1: MapSquareP = getSquare(b, P(4, 0))
    println("4:0 -----------------------")
    println(m1)
    assert(m1.sm.hv == HutVillage.Village)
  }
}