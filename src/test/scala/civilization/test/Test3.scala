package civilization.test

import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.objects._
import org.scalatest.funsuite.AnyFunSuite


class Test3 extends AnyFunSuite {

  Helper.X

  test("Test get hut from market") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME1.json")
    val prev: Int = b.resources.hv.length
    println(prev)
    assert(prev > 0)
    val prevu: Int = b.resources.hvused.length
    val hv: HutVillage = getRandomHutVillage(b, HutVillage.Hut)
    println(hv)
    assert(hv.hv == HutVillage.Hut)
    assert(b.resources.hvused.length == prevu)
    assert(b.resources.hv.length == (prev - 1))
  }

  test("reveal tile down") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME2.json")
    val ma: MapSquareP = getSquare(b, P(0, 0))
    assert(!ma.revealed)
    revealTile(b, Orientation.Left, P(0, 0))
    val ma1: MapSquareP = getSquare(b, P(0, 0))
    assert(ma1.revealed)
    // next
    val ma2: MapSquareP = getSquare(b, P(4, 0))
    println(ma2)
    assert(!ma2.revealed)
    revealTile(b, Orientation.Down, P(1, 0))
    val ma3: MapSquareP = getSquare(b, P(4, 0))
    println(ma3)
    assert(ma3.revealed)
    assert(ma3.s.hv.isDefined)
    assert(ma3.s.hv.get.hv == HutVillage.Village)
    val ma4: MapSquareP = getSquare(b, P(6, 3))
    println(ma4)
    assert(ma4.s.hv.isDefined)
    assert(ma4.s.hv.get.hv == HutVillage.Hut)
  }

  test("reveal tile left") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME2.json")
    println("---------------")
    revealTile(b, Orientation.Left, P(1, 0))
    val ma3: MapSquareP = getSquare(b, P(7, 0))
    println(ma3)
    assert(ma3.revealed)
    assert(ma3.s.hv.isDefined)
    assert(ma3.s.hv.get.hv == HutVillage.Village)
    val ma4: MapSquareP = getSquare(b, P(4, 2))
    println(ma4)
    assert(ma4.s.hv.isDefined)
    assert(ma4.s.hv.get.hv == HutVillage.Hut)
  }

  test("reveal tile up") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME2.json")
    println("---------------")
    revealTile(b, Orientation.Up, P(1, 0))
    val ma3: MapSquareP = getSquare(b, P(7, 3))
    println(ma3)
    assert(ma3.revealed)
    assert(ma3.s.hv.isDefined)
    assert(ma3.s.hv.get.hv == HutVillage.Village)
    val ma4: MapSquareP = getSquare(b, P(5, 0))
    println(ma4)
    assert(ma4.s.hv.isDefined)
    assert(ma4.s.hv.get.hv == HutVillage.Hut)
  }

  test("reveal tile right") {
    val b: GameBoard = Helper.getBoard("test2/BOARDGAME2.json")
    println("---------------")
    revealTile(b, Orientation.Right, P(1, 0))
    val ma3: MapSquareP = getSquare(b, P(4, 3))
    println(ma3)
    assert(ma3.revealed)
    assert(ma3.s.hv.isDefined)
    assert(ma3.s.hv.get.hv == HutVillage.Village)
    val ma4: MapSquareP = getSquare(b, P(7, 1))
    println(ma4)
    assert(ma4.s.hv.isDefined)
    assert(ma4.s.hv.get.hv == HutVillage.Hut)
  }

}
