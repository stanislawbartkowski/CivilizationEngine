package civilization.test

import civilization.I
import civilization.I.executeCommand
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue}


class Test31 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Russia start bonus") {
    val token: String = II.getData(II.REGISTEROWNER, "Russia")
    println(token)
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Russia)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "SETCAPITAL")
    println(ite)
    Helper.executeCommandH(token, "SETCAPITAL", 2, 2)

    gg = I.getBoardForToken(token)
    l = allowedCommands(gg, Civilization.Russia)
    println(l)
    ite = II.getData(II.ITEMIZECOMMAND, token, "SETARMY")
    println(ite)
    Helper.executeCommandH(token, "SETARMY", 2, 2, "{\"col\" : 2, \"row\" : 1}")

    gg = I.getBoardForToken(token)
    l = allowedCommands(gg, Civilization.Russia)
    println(l)
    // set army twice
    assert(l contains Command.SETARMY)
    Helper.executeCommandH(token, "SETARMY", 2, 2, "{\"col\" : 3, \"row\" : 3}")

    // again check
    gg = I.getBoardForToken(token)
    l = allowedCommands(gg, Civilization.Russia)
    println(l)
    // army limit exceeded
    assert(!(l contains Command.SETARMY))
    val li = getLimits(gg, Civilization.Russia)
    println(li.stackinglimit)
    // for Russia is 3
    assert(li.stackinglimit == 3)
    val no = getNumberOfArmies(gg, Civilization.Russia)
    println(no._1)
    // two armies
    assert(no._1 == 2)
  }

  test("Russia, sacrifice figures") {
    val reg = Helper.ReadAndPlayForTwo("test31/BOARDGAME1.json", "test31/PLAY1.json", Civilization.Russia, Civilization.Arabs)
    val tokenR = reg._1
    val tokenA = reg._2
    var gg = I.getBoardForToken(tokenR)
    var l = allowedCommands(gg, Civilization.Russia)
    println(l)
    assert(l contains Command.SACRIFICEFIGUREFORTECH)
    val s = II.getData(II.GETBOARDGAME, tokenR)
    val ju: JsValue = Helper.jyou(toJ(s))
    println(ju)
    var ite = II.getData(II.ITEMIZECOMMAND, tokenR, "SACRIFICEFIGUREFORTECH")
    val jite = toJ(ite).as[JsArray]
    assert(jite.value.length == 1)
    val jtech: JsArray = (jite.value(0) \ "tech").as[JsArray]
    assert(jtech.value.length == 3)
    println(jtech)
    println(jite)
    println(gg.playerDeck(Civilization.Russia).tech)
    println(gg.playerDeck(Civilization.Arabs).tech)
    Helper.executeCommandH(tokenR, "SACRIFICEFIGUREFORTECH", 12, 0, "\"Logistics\"")
    // verify
    gg = I.getBoardForToken(tokenR)
    val ma: MapSquareP = getSquare(gg, P(12, 0))
    println(ma.s.figures)
    // no figures
    assert(ma.s.figures.numberofArmies == 0 && ma.s.figures.numberofScouts == 0)
    assert(ma.civHere.isEmpty)
    // is tech added
    println(gg.playerDeck(Civilization.Russia).tech)
    assert(gg.playerDeck(Civilization.Russia).tech.map(_.tech) contains TechnologyName.Logistics)
    l = allowedCommands(gg, Civilization.Russia)
    println(l)
    // cannot sacrifice more figures
    assert(!(l contains Command.SACRIFICEFIGUREFORTECH))

  }
}


