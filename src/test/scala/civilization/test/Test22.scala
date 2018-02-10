package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.io.fromjson.ImplicitMiximFromJson
import org.scalatest.FunSuite
import civilization.io.readdir._
import civilization.objects._


class Test22 extends FunSuite with ImplicitMiximFromJson {

  Helper.I

  test("Wonders") {
    val token: String = II.getData(II.REGISTEROWNER, "China")
    println(token)
    var gg: GameBoard = I.getBoardForToken(token)
    println(gg.market.wonders)
    assert(12 == gg.market.wonders.length)
    assert(4 == gg.market.wonders.filter(w => GameResources.getWonder(w).age == WondersAge.Modern).length)
  }

}
