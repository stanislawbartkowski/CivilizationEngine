package civilization.test

import civilization.I.II
import civilization.I
import civilization.I.{executeCommand, registerGame}
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.objects.{Civilization, Command, P}
import org.scalatest.FunSuite
import Helper._



class Test12 extends FunSuite {

  Helper.I

  test("Spend trade for production") {
    var b: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test12/GAME1.json", Civilization.Rome)
    val token: String = registerGame(b, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var prod = numberofTradeCalculateH(b, g.playerDeck(Civilization.Rome))
    println(prod)
    assert(4 == prod.trade)
    var prodc = getProductionForCityH(g, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(6 == prodc.prod)
    Helper.executeCommandH(token, "SPENDTRADE", 2, 2, "1")
    b = I.getBoardForToken(token)
    prod = numberofTradeCalculateH(b, b.playerDeck(Civilization.Rome))
    println(prod)
    assert(4 == prod.terrain)
    assert(3 == prod.toprod)
    assert(1 == prod.trade)
    var s = executeCommand(token, "SPENDTRADE", 2, 2, "1")
    println(s)
    assert(s != null)
    prodc = getProductionForCityH(b, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(7 == prodc.prod)
    assert(1 == prodc.fromtrade)
    Helper.executeCommandH(token, "UNDOSPENDTRADE", 2, 2, null)
    b = I.getBoardForToken(token)
    prod = numberofTradeCalculateH(b, b.playerDeck(Civilization.Rome))
    println(prod)
    assert(4 == prod.trade)
    prodc = getProductionForCityH(b, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(6 == prodc.prod)
    // again
    Helper.executeCommandH(token, "SPENDTRADE", 2, 2, "1")
    b = I.getBoardForToken(token)
    prod = numberofTradeCalculateH(b, b.playerDeck(Civilization.Rome))
    println(prod)
    assert(1 == prod.trade)
    prodc = getProductionForCityH(b, Civilization.Rome, P(2, 2))
    println(prodc)
    assert(7 == prodc.prod)
  }

  test("Spend allowed commands") {
    var b: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test12/GAME1.json", Civilization.Rome)
    val token: String = registerGame(b, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(b, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.SPENDTRADE).isDefined)
    Helper.executeCommandH(token, "SPENDTRADE", 2, 2, "1")
    b = I.getBoardForToken(token)
    l = allowedCommandsH(b, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.UNDOSPENDTRADE).isDefined)
    Helper.executeCommandH(token, "UNDOSPENDTRADE", 2, 2, null)
    b = I.getBoardForToken(token)
    l = allowedCommandsH(b, Civilization.Rome)
    println(l)
    assert(l contains Command.SPENDTRADE)
    assert(l contains Command.BUYSCOUT)
    assert(l contains Command.BUYARMY)
  }

  test("Spend and allow to buy") {
    var b: GameBoard = Helper.readBoardAndPlay("test5/BOARDGAME1.json", "test12/GAME2.json", Civilization.Germany)
    val token: String = registerGame(b, Civilization.Germany)
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(b, Civilization.Germany)
    assert(l.find(_ == Command.BUYSCOUT).isEmpty)
    println(l)
    var prodc = getProductionForCityH(b, Civilization.Germany, P(2, 2))
    println(prodc)
    Helper.executeCommandH(token, "SPENDTRADE", 2, 2, "1")
    g = I.getBoardForToken(token)
    l = allowedCommandsH(g, Civilization.Germany)
    var s: String = II.itemizeCommand(token, "SPENDTRADE")
    assert(s != null)
    println(s)
    // can buy scout
    println(l)
    assert(l.find(_ == Command.BUYSCOUT).isDefined)
    s = II.itemizeCommand(token, "UNDOSPENDTRADE")
    println(s)
    assert(s != null)
  }

  test("Check trade in RESEARCH") {
    val b: GameBoard = Helper.readBoardAndPlay("test5/BOARDGAME1.json", "test12/GAME3.json", Civilization.Germany)
    val token: String = registerGame(b, Civilization.Germany)
    var g: GameBoard = I.getBoardForToken(token)
    var tra : TradeForCivCalculate = numberofTradeCalculateH(g, g.playerDeck(Civilization.Germany))
    println(tra)
    // trade spending should be included
    assert(6 == tra.trade)
    assert(3 == tra.toprod)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, "\"Research\"")
    // reset spending at then beginning of next round
    g = I.getBoardForToken(token)
    tra  = numberofTradeCalculateH(g, Civilization.Germany)
    println(tra)
    assert(9 == tra.trade)
  }
}
