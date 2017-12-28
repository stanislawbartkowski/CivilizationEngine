package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson._
import civilization.objects.{Civilization, Command, _}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue, _}

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
    val s : String = II.itemizeCommand(token,"HARVESTRESOURCE")
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
    val bs: String = II.getData(II.GETBOARDGAME, token)
    val j : JsValue = toJ(bs)
    println(Json.prettyPrint(j))
    val r : JsValue = (j \ "board" \ "resources").get
    println(r)
  }

  test("Test explore huts scout cannot explore hut") {
    val reg = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test16/GAME3.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    val ma : MapSquareP = getSquare(g,P(6,2))
    println(ma)
    assert(HutVillage.Hut == ma.s.hv.get.hv)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    // scout, cannot explore
    assert(l.find(_ == Command.EXPLOREHUT).isEmpty)
  }

  test("Test explore huts, army explore hut") {
    val reg = Helper.readBoardAndPlayT("test9/BOARDGAME1.json", "test16/GAME4.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    val ma : MapSquareP = getSquare(g,P(6,2))
    println(ma)
    assert(HutVillage.Hut == ma.s.hv.get.hv)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    // army can explore
    assert(l.find(_ == Command.EXPLOREHUT).isDefined)
    val i : String = II.itemizeCommand(token,"EXPLOREHUT")
    val j : JsValue = toJ(i)
    println(i)
    // verify: from (5,2) can explore hut (6,2)
    val p = (j \ "p").get.as[P]
    assert(P(5,2) == p)
    val li : Seq[P] = (j \ "explore").get.as[Seq[P]]
    assert(P(6,2) == li(0))
    Helper.executeCommandH(token, "EXPLOREHUT", 6,2,null)
    // verify
    g  = I.getBoardForToken(token)
    // check square
    val ma1 : MapSquareP = getSquare(g,P(6,2))
    println(ma1)
    // hut should have dissapeared
    assert(!ma1.s.hvhere)
    // fugure should be at this point
    assert(1 == ma1.s.figures.numberofArmies)
    // figure removed from previous position
    val ma2 : MapSquareP = getSquare(g,P(5,2))
    println(ma2)
    assert(ma2.s.figures.empty)
    val ge  = g.playerDeck(Civilization.Rome)
    println(ge.hvlist)
    assert(1 == ge.hvlist.length)
    val bs: String = II.getData(II.GETBOARDGAME, token)
    // verify JSON player deck
    val jj : JsValue = toJ(bs)
    println(Json.prettyPrint(jj))
    val hutvi = (jj \ "board" \ "you" \ "hutvillages").get
    println (hutvi)
    val laa : JsArray = (hutvi \ "list").get.as[JsArray]
    println(laa)
    assert(1 == laa.value.length)
  }

}
