package civilization.test

import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, P}
import org.scalatest.funsuite.AnyFunSuite
import civilization.test.Helper._
import civilization.gameboard.GameBoard
import civilization.helper.{TradeForCiv, numberofTrade}

class Test40  extends AnyFunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.X

  test("Verify EXPLOREHUT") {
    val (token, gameid) = Helper.readGameSingle("test40/GAME1.json", Civilization.America)
    Helper.executeCommandH(token, "EXPLOREHUT",0,4)
    II.deleteGame(gameid)
  }

  test("Verify MOVE without jsparam") {
    val (token, gameid) = Helper.readGameSingle("test40/GAME1.json", Civilization.America)
    Helper.executeCommandH(token, "MOVE",2,4)
    II.deleteGame(gameid)
  }


}
