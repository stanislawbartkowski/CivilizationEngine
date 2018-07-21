package civilization.test

import civilization.I
import civilization.gameboard.GameBoard
import civilization.helper.{MapSquareP, getSquare}
import civilization.io.fromjson.toJ
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects._
import org.scalatest.FunSuite
import Helper._


class Test18 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("Failed battle with village") {

    val reg = Helper.readBoardAndPlayT("test18/BOARDGAME1.json", "test18/PLAY1.json", Civilization.America)
    val token: String = reg._1
    val s = II.getData(II.GETBOARDGAME, token)
    val js = toJ(s)
    val batt = (js \ "board" \ "battle").get
    println(batt)
    Helper.checkendofbattle(batt, true)
    Helper.checkattackerwinner(batt, false)
    // submit ENDBATTLE
    Helper.executeCommandH(token, "ENDBATTLE", -1, -1, """[]""")
    // battle is lost
    // verify
    val gg: GameBoard = I.getBoardForToken(token)
    var ma: MapSquareP = getSquare(gg, P(3, 7))
    // no figures at attacking square
    assert(ma.s.figures.empty)
    // no civilization there
    assert(ma.civHere.isEmpty)
    // village still exists
    ma = getSquare(gg, P(2, 7))
    assert(ma.s.hv.get.hv == HutVillage.Village)
    // number of units
    println(gg.playerDeck(Civilization.America).units)
    // one unit survived
    assert(gg.playerDeck(Civilization.America).units.length == 1)
    // no battle
    assert(gg.battle.isEmpty)
    val l = allowedCommandsH(gg, Civilization.America)
    println(l)
    assert(l.filter(_ == Command.MOVE).isEmpty)
    assert(l.filter(_ == Command.ENDOFMOVE).isEmpty)
    assert(!l.filter(_ == Command.ENDOFPHASE).isEmpty)
  }


  test("Error while attacking village having two figures") {
    val reg = Helper.readBoardAndPlayT("test18/BOARDGAME2.json", "test18/PLAY2.json", Civilization.Germany)
    val token: String = reg._1
    Helper.executeCommandH(token, "ENDBATTLE", -1, -1, """[]""")
    val gg: GameBoard = I.getBoardForToken(token)
    var ma: MapSquareP = getSquare(gg, P(4, 1))
    println(ma)
    println(ma.s.figures)
    assert(ma.s.figures.empty)
    // now check conquered village
    ma = getSquare(gg, P(5, 1))
    println(ma)
    println(ma.s.figures)
    // one figure
    assert(1 == ma.s.figures.numberofArmies)
    // village wiped out
    assert(!ma.s.hvhere)
  }

  test("Error while two players game") {
    val reg = Helper.ReadAndPlayForTwo("test18/BOARDGAME3.json", "test18/PLAY3.json", Civilization.America, Civilization.China)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    assert(!gg.battle.get.attackerwinner)
    assert(gg.battle.get.endofbattle)
    Helper.executeCommandH(token, "ENDBATTLE", -1, -1, """[]""")
    gg = I.getBoardForToken(token)
    var ma: MapSquareP = getSquare(gg, P(12, 4))
    println(ma)
    println(ma.s.figures)
    // atacking figure killed
    // 2 figures on the square, one left
    assert(ma.s.figures.numberofArmies == 1)
    ma = getSquare(gg, P(12, 3))
    // village still exists
    assert(ma.civHere.isEmpty)
    assert(ma.s.hvhere)
  }

}
