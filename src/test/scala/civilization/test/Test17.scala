package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.io.fromjson._
import civilization.io.readdir.GenBoard.genBoard
import civilization.objects.{Civilization, Command}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue}
import civilization.objects._

class Test17 extends FunSuite {

  Helper.I

  test("Test explore hut") {
    val reg = Helper.readBoardAndPlayT("test17/BOARD1.json", "test17/GAME1.json", Civilization.America)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.America)
    println(l)
    val i: String = II.itemizeCommand(token, "EXPLOREHUT")
    val j: JsValue = toJ(i)
    println(i)
    Helper.executeCommandH(token, "EXPLOREHUT", 6, 3, null)
    g = I.getBoardForToken(token)
    l = allowedCommands(g, Civilization.America)
    println(l)
    assert(l.find(_ == Command.MOVE).isEmpty)
    assert(l.find(_ == Command.REVEALTILE).isEmpty)
    assert(l.find(_ == Command.ENDOFMOVE).isEmpty)
    assert(l.find(_ == Command.ENDOFPHASE).isDefined)
  }

  test("Gen BoardGJ, check that resources for hut and villages are preserved") {

    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    var noh: Int = 0
    g.map.map.foreach(
      m => m.mapsquares.foreach(mm => mm.foreach(
        ss => {
          println(ss)
          if (ss.hvhere && ss.hv.get.resource != null) noh = noh + 1
        }
      ))
    )
    println(noh)
    assert(noh > 0)
    val token: String = civilization.I.registerGame(g, Civilization.Germany)
    var gg: GameBoard = I.getBoardForToken(token)
    var nohh = 0
    gg.map.map.foreach(
      m => m.mapsquares.foreach(mm => mm.foreach(
        ss => {
          println(ss)
          if (ss.hvhere && ss.hv.get.resource != null) nohh = nohh + 1
        }
      ))
    )
    println(nohh)
    assert(noh == nohh)
    //    val bs: String = II.getData(II.GETBOARDGAME, token)
    //    println(bs)
  }

  test("Test Attack Village") {
    val reg = Helper.readBoardAndPlayT("test17/BOARD2.json", "test17/GAME2.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    val l = allowedCommands(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.ATTACK).isDefined)
    val i: String = II.itemizeCommand(token, "ATTACK")
    val j: JsValue = toJ(i)
    println(i)
    val a : JsArray = (j \ "attack").as[JsArray]
    println(a)
    val p : P = a.value.head.as[P]
    assert(P(4,0) == p)
    Helper.executeCommandH(token, "ATTACK", 4, 0, null)
    // all units in battle
    g = I.getBoardForToken(token)
    assert(g.market.units.isEmpty)
    val pl : PlayerDeck = g.playerDeck(Civilization.Rome)
    assert (pl.units.isEmpty)
  }

}
