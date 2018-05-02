package civilization.test

import civilization.I
import civilization.I.II
import civilization.helper.AllowedCommands.allowedCommands
import civilization.helper._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.objects.Command
import civilization.objects._


class Test27 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("Check Currency") {

    val reg = Helper.readBoardAndPlayT("test27/BOARDGAME1.json", "test27/PLAY1.json", Civilization.America)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.America)
    println(l)
    assert(l contains Command.CURRENCYACTION)
    var ite = II.getData(II.iTEMIZECOMMAND, token, "CURRENCYACTION")
    println(ite)
    // run command
    val param = """{"resource" : "Incense"}"""
    Helper.executeCommandH(token, "CURRENCYACTION", 2,2,param)
    // check culture
    gg = I.getBoardForToken(token)
    val culture = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    println(culture)
    assert(culture == 3)
    // incense, 0
    val incense = gg.playerDeck(Civilization.America).resou.nof(Resource.Incense)
    println(incense)
    assert(incense == 0)
    val co = getCoins(gg,Civilization.America)
    println(co)
    assert(co.coins == 0)

  }
}
