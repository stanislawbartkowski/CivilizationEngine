package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.io.fromjson.toJ
import civilization.io.readdir.GenBoard.genBoard
import civilization.objects.{Civilization, Command}
import org.scalatest.FunSuite
import play.api.libs.json.JsValue

class Test17  extends FunSuite {

  Helper.I

  test("Test explore hut") {
    val reg = Helper.readBoardAndPlayT("test17/BOARD1.json", "test17/GAME1.json", Civilization.America)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommands(g, Civilization.America)
    println(l)
    val i : String = II.itemizeCommand(token,"EXPLOREHUT")
    val j : JsValue = toJ(i)
    println(i)
    Helper.executeCommandH(token, "EXPLOREHUT", 6,3,null)
    g = I.getBoardForToken(token)
    l  = allowedCommands(g, Civilization.America)
    println(l)
    assert(l.find(_ == Command.MOVE).isEmpty)
    assert(l.find(_ == Command.REVEALTILE).isEmpty)
    assert(l.find(_ == Command.ENDOFMOVE).isEmpty)
    assert(l.find(_ == Command.ENDOFPHASE).isDefined)
  }

  test("Gen BoardGJ, check that resources for hut and villages are preserved") {

    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    var noh : Int = 0
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

  }
