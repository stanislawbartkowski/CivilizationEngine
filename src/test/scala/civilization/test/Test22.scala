package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.readdir._
import civilization.objects._
import org.scalatest.FunSuite


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

  test("Upgrade building") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME1.json", "test22/PLAY1.json", Civilization.Arabs)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val p  = getSquare(gg,P(2,1))
    println(p)
    // should be upgrade to IronMine
    assert(p.s.building.get.name == BuildingName.IronMine)
  }

  test("Research wonder") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME2.json", "test22/PLAY2.json", Civilization.Germany)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val ss = II.getData(II.GETBOARDGAME, token)
    println(ss)
    // should work without exception
  }

  test("Buy wonder") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME2.json", "test22/PLAY3.json", Civilization.Germany)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.BUYWONDER).isDefined)
    val ss = II.itemizeCommand(token,"BUYWONDER")
    println(ss)
    // buy wonder
    val c =
      """{"p":{"row":1,"col":1},"wonder":"Stonehenge"}"""
    Helper.executeCommandH(token, "BUYWONDER", 2, 2, c)
    gg = I.getBoardForToken(token)
    val p : MapSquareP = getSquare(gg,P(1,1))
    println(p)
    assert(p.s.wonder.get.w == Wonders.Stonehenge)
    // test wonders
    println(gg.market.wonders)
    // Stonehege should dissapear
    assert(gg.getCurrentWonders().find(_ == Wonders.Stonehenge).isEmpty)
  }

  }
