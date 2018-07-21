package civilization.test

import civilization.I
import civilization.I.executeCommand
import civilization.gameboard.{GameBoard, WinnerLoot}
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._
import civilization.helper.battle.BattleActions


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
    checkendofbattle(batt, true)
    checkattackerwinner(batt, true)
    // issue end of battle immediately
    // take a loot
    val param =
    """ [{"name" : "resource","loot" : 1,"resource" : "Incense"}]  """
    Helper.executeCommandH(tokenR, "ENDBATTLE", -1, -1, param)
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
    val cities: Seq[MapSquareP] = citiesForCivilization(gg, Civilization.Arabs)
    cities.foreach(println)
    val trade = numberofTradeCalculateH(gg, Civilization.Arabs)
    println(trade.tradefromGreatLight)
    assert(trade.tradefromGreatLight == 1)
    val tr = numberofTradeH(gg, Civilization.Arabs)
    println(tr.trade)
    // should be 9, one more
    println(tr.trade == 9)
  }

  test("Two players game, win culture") {
    val reg = Helper.ReadAndPlayForTwo("test33/BOARDGAME4.json", "test33/PLAY4.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    val cultA = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    val cultC = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cultA)
    println(cultC)
    val s: WinnerLoot = BattleActions.winnerLoot(gg)
    println(s)
    val param = """ [{"name" : "culture","loot" : 1}]  """
    Helper.executeCommandH(tokenA, "ENDBATTLE", -1, -1, param)
    gg = I.getBoardForToken(tokenA)
    val cultAA = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    val cultCC = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cultAA)
    println(cultCC)
    assert(cultA + 3 == cultAA)
    assert(cultC - 3 == cultCC)
  }


  test("Two players game, drop culture card") {
    val reg = Helper.ReadAndPlayForTwo("test33/BOARDGAME4.json", "test33/PLAY4.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    val cardlistp = gg.playerDeck(Civilization.China).cultureresource.cards
    println(cardlistp)
    assert(!cardlistp.isEmpty)
    val s: WinnerLoot = BattleActions.winnerLoot(gg)
    println(s)
    val param = """ [{"name" : "card","loot" : 1,"level" : 1}]  """
    Helper.executeCommandH(tokenA, "ENDBATTLE", -1, -1, param)
    gg = I.getBoardForToken(tokenA)
    val cardlist = gg.playerDeck(Civilization.China).cultureresource.cards
    println(cardlist)
    assert(cardlist.isEmpty)
    val cardlista = gg.playerDeck(Civilization.America).cultureresource.cards
    println(cardlista)
    assert(cardlista.isEmpty)

  }

  test("Two players game, drop coin") {
    val reg = Helper.ReadAndPlayForTwo("test33/BOARDGAME4.json", "test33/PLAY5.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    val s: WinnerLoot = BattleActions.winnerLoot(gg)
    val coins = gg.playerDeck(Civilization.America).tech.find(_.tech == TechnologyName.Pottery).get.coins
    val ee = getCoins(gg, gg.playerDeck(Civilization.America))
    println(ee.coins)
    println(s)
    println(coins)
    val param = """ [{"name" : "coin","loot" : 1,"tech" : "Pottery"}]  """
    Helper.executeCommandH(tokenA, "ENDBATTLE", -1, -1, param)
    gg = I.getBoardForToken(tokenA)
    val coins1 = gg.playerDeck(Civilization.America).tech.find(_.tech == TechnologyName.Pottery).get.coins
    assert(coins1 == 0)
    val e = getCoins(gg, gg.playerDeck(Civilization.America))
    println(e.coins)
    assert(ee.coins - 1 == e.coins)
  }

  test("Two players game, attack the city") {
    val reg = Helper.ReadAndPlayForTwo("test33/BOARDGAME4.json", "test33/PLAY6.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l contains Command.ATTACK)
    var ite = II.getData(II.ITEMIZECOMMAND, tokenC, "ATTACK")
    println(ite)
    Helper.executeCommandH(tokenC, "ATTACK", 6, 2)
    gg = I.getBoardForToken(tokenA)
    println(gg.battle.get.defender.combatBonus)
    // city is defended
    assert(gg.battle.get.defender.combatBonus == 6)
    println(gg.battle.get.defender.waiting)
    assert(gg.battle.get.defender.waiting.length == 3)
    println(gg.battle.get.attacker.waiting)
    assert(gg.battle.get.attacker.waiting.length == 3)
  }

  test("Two players game, attack the city and lose, only single loot") {
    val reg = Helper.ReadAndPlayForTwo("test33/BOARDGAME4.json", "test33/PLAY7.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    val s: WinnerLoot = BattleActions.winnerLoot(gg)
    println(s)
    // not 2
    assert(s.loot == 1)
  }

  test("Two players game, China lost battle with city, verify number of units") {
    val reg = Helper.ReadAndPlayForTwo("test33/BOARDGAME4.json", "test33/PLAY8.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenA)
    val cultaa = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    println(cultaa)
    val cultcc = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cultcc)

    //    println(gg.playerDeck(Civilization.China).units)
    val param = """ [{"name" : "culture","loot" : 1}]  """
    Helper.executeCommandH(tokenA, "ENDBATTLE", -1, -1, param)
    gg = I.getBoardForToken(tokenA)
    val culta = gg.playerDeck(Civilization.America).resou.nof(Resource.Culture)
    println(culta)
    assert(culta == cultaa + 3)
    val cultc = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(cultc)
    assert(cultc == cultcc -3)

    // China: two survived and 1 saved
    val unitC =  gg.playerDeck(Civilization.China).units
    println(unitC)
    assert(unitC.length == 3)
  }
}