package civilization.test

import civilization.I
import civilization.objects._
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.objects.{Civilization, Command}
import org.scalatest.FunSuite
import civilization.helper._
import play.api.libs.json.JsArray
import civilization.io.fromjson.toJ


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

  test("Harvest resource") {
    val reg = Helper.readBoardAndPlayT("test16/BOARDGAME1.json", "test16/GAME2.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    println(g.resources.resou.table)
    assert(2 == g.resources.resou.nof(Resource.Iron))
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.HARVESTRESOURCE).isDefined)
    val s : String = I.itemizeCommand(token,"HARVESTRESOURCE")
    println(s)
    val a :JsArray = toJ(s).as[JsArray]
    assert(3 == a.value.length)
    Helper.executeCommandH(token, "HARVESTRESOURCE", 2, 2, """{ "row" : 2,"col" : 3}""")
    g = I.getBoardForToken(token)
    l= allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.HARVESTRESOURCE).isEmpty)
    val reso = g.playerDeck(Civilization.Rome).resou
    println(reso.table)
    assert(1 == reso.nof(Resource.Iron))
    assert(1 == g.resources.resou.nof(Resource.Iron))
  }

}
