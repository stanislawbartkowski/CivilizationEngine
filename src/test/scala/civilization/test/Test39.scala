package civilization.test

import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, P}
import org.scalatest.funsuite.AnyFunSuite
import civilization.test.Helper._
import civilization.gameboard.GameBoard
import civilization.helper.{TradeForCiv, numberofTrade}


class Test39 extends AnyFunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.X

  private def verify1(token: String, expectedtrade: Int, expectedprod: Int) = {
    val g: GameBoard = Helper.getBoardToken(token)
    val t: TradeForCiv = numberofTradeH(g, Civilization.America)
    println(t.trade)
    println(t)
    assert(t.trade == expectedtrade)
    var prodc = getProductionForCityH(g, Civilization.America, P(2, 2))
    println(prodc.prod)
    assert(prodc.prod == expectedprod)
  }

  test("Verify again SPENDTRADE") {
    val (token, gameid) = Helper.readGameSingle("test39/GAME1.json", Civilization.America)
    verify1(token, 7, 5)
    Helper.executeCommandH(token, "SPENDTRADE", 2, 2, "1")
    verify1(token, 5, 6)
    II.deleteGame(gameid)
  }


  test("Test incorrect ADVANCECULTURE") {
    val (token, gameid) = Helper.readGameSingle("test39/GAME2.json", Civilization.America)
    // command should fail
    Helper.executeCommandFail(token, "ADVANCECULTURE")
  }

  test("Verify GETBOARD twice") {
    val (token, gameid) = Helper.readGameSingle("test39/GAME1.json", Civilization.America)
    verify1(token, 7, 5)
    Helper.executeCommandH(token, "SPENDTRADE", 2, 2, "1")
    val res = II.getData(II.GETBOARDGAME,token)
    print(res)
    // try again
    val res1 = II.getData(II.GETBOARDGAME,token)
    print("\n=============================================================\n")
    print(res1)
    // empty for the second time
    assert(res1 == "")
    II.deleteGame(gameid)
  }

}

