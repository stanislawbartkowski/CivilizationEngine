package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.gameboard._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir._
import civilization.objects._
import org.scalatest.FunSuite
import play.api.libs.json._
import civilization.io.fromjson.{toJ, _}
import civilization.io.readdir.GenBoard.genBoard
import civilization.test.Helper._


class Test23 extends FunSuite with ImplicitMiximFromJson {

  Helper.I

  test("Economy from outskirt") {
    val reg = Helper.readBoardAndPlayT("test23/BOARDGAME1.json", "test23/PLAY1.json", Civilization.Germany)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val e = getCoins(gg, Civilization.Germany)
    println(e)
    assert(e.squares == 1)
    assert(e.coins == 1)
  }

  test("Economy from outskirt and scout on outskirt") {
    val reg = Helper.readBoardAndPlayT("test23/BOARDGAME1.json", "test23/PLAY2.json", Civilization.Germany)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val e = getCoins(gg, Civilization.Germany)
    println(e)
    assert(e.squares == 1)
    assert(e.coins == 1)
    val j = Helper.getB(token)
    val y = jyou(j)
    println(y)
    val c = (y \ "coins").as[Int]
    println(c)
    assert(c == 1)
    val bonus = (y \ "combatbonus").as[Int]
    println(bonus)
    assert(bonus == 0)

  }

  test("Trade from building") {
    val reg = Helper.readBoardAndPlayT("test23/BOARDGAME3.json", "test23/PLAY3.json", Civilization.Spain)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val p = getProductionForCity(gg, Civilization.Spain, P(2, 2))
    println(p)
    assert(p.prod == 4)
    val t: TradeForCiv = numberofTrade(gg, Civilization.Spain)
    println(t)
    // buidling shoud be included
    assert(t.trade == 5)
  }

  test("Combat bonus for baracks") {
    val reg = Helper.readBoardAndPlayT("test23/BOARDGAME4.json", "test23/PLAY4.json", Civilization.Spain)
    val token = reg._1
    val j = Helper.getB(token)
    val y = jyou(j)
    println(y)
    val bonus = (y \ "combatbonus").as[Int]
    println(bonus)
    // barracks is built
    assert(bonus == 2)
  }

}