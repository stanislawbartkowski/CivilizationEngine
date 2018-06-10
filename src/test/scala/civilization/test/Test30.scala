package civilization.test

import civilization.I
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue}


class Test30 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Germany bonus") {
    val token: String = II.getData(II.REGISTEROWNER, "Germany")
    println(token)
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Germany)
    println(pl.units.length)
    assert(pl.units.length == 5)
    // three infantry
    assert(pl.units.filter(_.utype == CombatUnitType.Infantry).length == 3)
  }

  test("Technology upgrade unit") {
    val reg = Helper.readBoardAndPlayT("test30/BOARDGAME1.json", "test30/PLAY1.json", Civilization.Germany)
    val token = reg._1
    Helper.executeCommandH(token, "RESEARCH", -1, -1,""""Democracy"""")
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Germany)
    CombatUnitType.values.foreach(m => {
      println(pl.combatlevel.getStrength(m))
    })
    // infantry upgraded
    assert(pl.combatlevel.getStrength(CombatUnitType.Infantry) == 1)
    println(pl.units.length)
    // additional unit
    assert(pl.units.length == 6)
    println(pl.takefreeResources)
    assert(pl.takefreeResources)
    var l = allowedCommands(gg, Civilization.Germany)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "GETFREERESOURCE")
    println(ite)
    Helper.executeCommandH(token, "GETFREERESOURCE", -1, -1,""""Incense"""")
    gg = I.getBoardForToken(token)
    val noi = gg.playerDeck(Civilization.Germany).resou.nof(Resource.Incense)
    assert(noi == 1)
    val pl1 = gg.playerDeck(Civilization.Germany)
    println(pl1.takefreeResources)
    assert(!pl1.takefreeResources)
  }
}