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


class Test28 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("America bonus") {
    val gg = getBoardAndRegister("test28/GAMEBOARD1.json", Civilization.America)
    val g: GameBoard = gg._2
    val token: String = gg._1
    val pl: PlayerLimits = getLimits(g, Civilization.America)
    println(pl.tradeforProd)
    assert(pl.tradeforProd == 3)
    assert(pl.prodfortrade == 2)
    val s = II.getData(II.GETBOARDGAME, token)
    val ju: JsValue = Helper.jyou(toJ(s))
    println(ju)
    val prodfortrade = (ju \ "prodfortrade").as[Int]
    println(prodfortrade)
    assert(prodfortrade == 2)
  }

  test("Generate board for America") {
    val token: String = II.getData(II.REGISTEROWNER, "America")
    println(token)
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.America)
    println(pl.cultureresource.persons.length)
    // great person at the beginning
    assert(pl.cultureresource.persons.length == 1)
  }

  test("Spend trade ratio for America") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD1.json", "test28/PLAY1.json", Civilization.America)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val prod = getProductionForCity(gg,Civilization.America,P(2,2))
    println(prod)
    assert(prod.prod == 7)
    // increased by 2
    assert(prod.fromtrade == 2)
  }

}
