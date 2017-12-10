package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper.{MapSquareP, getSquare}
import civilization.io.fromjson.toJ
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects._
import org.scalatest.FunSuite

class Test18 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("Failed battle with village") {

    val reg = Helper.readBoardAndPlayT("test18/BOARDGAME1.json", "test18/PLAY1.json", Civilization.America)
    val token: String = reg._1
    val s = II.getData(II.GETBOARDGAME, token)
    val js = toJ(s)
    val batt = (js \ "board" \ "battle").get
    println(batt)
    Helper.checkendofgame(batt, true)
    Helper.checkattackerwinner(batt, false)
    // submit ENDBATTLE
    Helper.executeCommandH(token, "ENDBATTLE", -1, -1, null)
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
    assert(gg.playerDeck(Civilization.America).units.isEmpty)
    // no battle
    assert(gg.battle.isEmpty)
    val l = allowedCommands(gg, Civilization.America)
    println(l)
    assert(l.filter(_ == Command.MOVE).isEmpty)
    assert(l.filter(_ == Command.ENDOFMOVE).isEmpty)
    assert(!l.filter(_ == Command.ENDOFPHASE).isEmpty)
  }


}
