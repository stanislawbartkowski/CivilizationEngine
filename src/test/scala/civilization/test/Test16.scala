package civilization.test

import civilization.I
import civilization.objects._
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.objects.{Civilization, Command}
import org.scalatest.FunSuite
import civilization.helper._


class Test16  extends FunSuite {

  Helper.I

  test("Test opposite player ending CityManagement") {
    val c = Helper.ReadAndPlayForTwo("test10/BOARDGAME2.json", "test16/GAME1.json", Civilization.Rome, Civilization.China)
    val tokenr = c._1
    val tokenc = c._2
    var g: GameBoard = I.getBoardForToken(tokenc)
    assert(2 == g.market.units.length)
    var prod = getProductionForCity(g,Civilization.China,P(5,1))
    println(prod)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.China)
    println(l)
    Helper.executeCommandH(tokenc, "BUYARTILLERY", 5, 1, null)
    g = I.getBoardForToken(tokenc)
    assert(1 == g.market.units.length)
    prod = getProductionForCity(g,Civilization.China,P(5,1))
    println(prod)
    // check again
    l = allowedCommands(g, Civilization.China)
    println(l)
    assert(l.find(_ == Command.BUYARTILLERY).isEmpty)
    assert(l.find(_ == Command.SPENDTRADE).isEmpty)
  }

}
