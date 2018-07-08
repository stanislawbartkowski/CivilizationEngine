package civilization.test

import civilization.I
import civilization.I.executeCommand
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._


class Test33 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Attack scout and take loot") {
    val reg = Helper.ReadAndPlayForTwo("test33/BOARDGAME1.json", "test33/PLAY1.json", Civilization.America, Civilization.Russia)
    val tokenA = reg._1
    val tokenR = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    // before battle
    val deckA = gg.playerDeck(Civilization.America).units.length
    println(deckA)
    val deckR = gg.playerDeck(Civilization.Russia).units.length
    println(deckR)
    // attack scout
    Helper.executeCommandH(tokenR, "ATTACK", 8, 1)
    val b = getB(tokenR)
    val batt = getBattle(b)
    println(batt)
    // scouts attacked, end of battle
    checkendofbattle(batt,true)
    checkattackerwinner(batt,true)
    // issue end of battle immediately
    // take a loot
    Helper.executeCommandH(tokenR, "ENDBATTLE", -1, -1,""" "Incense" """)
    gg = I.getBoardForToken(tokenA)
    // no battle
    assert(gg.battle.isEmpty)
    val deckA1 = gg.playerDeck(Civilization.America).units.length
    println(deckA1)
    val deckR1 = gg.playerDeck(Civilization.Russia).units.length
    println(deckR1)
    assert(deckA == deckA1)
    assert(deckR == deckR1)
  }

  test("Chivalry action") {
    val reg = Helper.readBoardAndPlayT("test33/BOARDGAME2.json", "test33/PLAY2.json", Civilization.Arabs)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Arabs)
    println(l)
    assert(l contains Command.CHIVALRYACTION)
    val cul = gg.playerDeck(Civilization.Arabs).resou.nof(Resource.Culture)
    println(cul)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "CHIVALRYACTION")
    println(ite)
    val param =
      """{"resource" : "Incense"}"""
    Helper.executeCommandH(token, "CHIVALRYACTION", 2, 6, param)
    gg = I.getBoardForToken(token)
    val cul1 = gg.playerDeck(Civilization.Arabs).resou.nof(Resource.Culture)
    println(cul1)
    assert(cul + 5 == cul1)
    val ince = gg.playerDeck(Civilization.Arabs).resou.nof(Resource.Incense)
    println(ince)
    assert(ince == 0)
  }

  test("GreatLightHouse") {
    val reg = Helper.readBoardAndPlayT("test33/BOARDGAME2.json", "test33/PLAY3.json", Civilization.Arabs)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val cities : Seq[MapSquareP] = citiesForCivilization(gg,Civilization.Arabs)
    cities.foreach(println)
    val trade = numberofTradeCalculateH(gg, Civilization.Arabs)
    println(trade.tradefromGreatLight)
    assert(trade.tradefromGreatLight == 1)
    val tr = numberofTradeH(gg, Civilization.Arabs)
    println(tr.trade)
    // should be 9, one more
    println(tr.trade == 9)
  }
}

