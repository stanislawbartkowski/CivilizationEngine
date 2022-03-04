package civilization.test

import civilization.I
import civilization.gameboard.{GameBoard, WinnerLoot}
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II}
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._
import civilization.io.readdir.GameResources


class Test34 extends AnyFunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.X

  test("Construction action") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME1.json", "test34/PLAY1.json", Civilization.China)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val s: Seq[P] = CityAvailableForAction(gg, Civilization.China)
    println(s)
    // CONSTRUCIONACTION, does not block city action
    assert(s.length == 3)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(!(l contains Command.CONSTRUCTIONACTION))
  }

  test("Culture victory") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME1.json", "test34/PLAY2.json", Civilization.China)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    Helper.executeCommandH(token, "ADVANCECULTURE", -1, -1)
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.China)
    println(l)
    // end of game
    assert(l.isEmpty)
    assert(gg.endofgame.isDefined)
    println(gg.endofgame.get)
    assert(gg.endofgame.get.winner == Civilization.China)
    assert(gg.endofgame.get.wintype == GameWinType.Culture)
    val b = toJ(II.getData(II.GETBOARDGAME, token))
    println(b)
    val e = (b \ "board" \ "endofgame")
    println(e)
    val c1 = (e \ "winner").as[String]
    val c2 = (e \ "wintype").as[String]
    assert(c1 == "China")
    assert(c2 == "Culture")
  }

  test("Wrong with technplogy") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME3.json", "test34/PLAY3.json", Civilization.Egypt)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "PHILOSOPHYACTION")
    println(ite)
    var li = gg.playerDeck(Civilization.Egypt).hvlist
    println(li)
    assert(li.length == 5)
    val param =
      """[{"hv" : "Hut", "resource" : "Incense"},{"hv" : "Hut", "resource" : "Spy"},{"hv" : "Hut","resource":"Spy"}]"""
    Helper.executeCommandH(token, "PHILOSOPHYACTION", 2, 2, param)
    gg  = I.getBoardForToken(token)
    li = gg.playerDeck(Civilization.Egypt).hvlist
    println(li)
    // drop three resources
    assert(li.length == 2)
    // no spies
    assert(li.filter(_.resource == Resource.Spy).isEmpty)

  }

  test("Exception was thrown, Academy key not found") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME3.json", "test34/PLAY4.json", Civilization.Egypt)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    // no exception
  }

  test("Technology victory, level 5") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME3.json", "test34/PLAY5.json", Civilization.Egypt)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "RESEARCH")
    println(ite)
    val le = techologyLevel(gg,gg.playerDeck(Civilization.Egypt))
    println(le)
    assert(le contains 5)
    val li = listOfRemainingTechnologies(gg,gg.playerDeck(Civilization.Egypt),le)
    println(li)
    li.foreach(t => {
      val te = GameResources.getTechnology(t)
      println(te)
      assert(te.level != 4 && te.level != 3)
    })
  }

  test("Technology victory") {
    val reg = Helper.readBoardAndPlayT("test34/BOARDGAME3.json", "test34/PLAY6.json", Civilization.Egypt)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    Helper.executeCommandH(token, "ENDOFPHASE", 2, 2, """ "Movement" """)
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Egypt)
    println(l)
    // end of game
    assert(l.isEmpty)
    assert(gg.endofgame.isDefined)
  }

  }