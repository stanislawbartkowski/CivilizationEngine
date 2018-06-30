package civilization.test

import civilization.I
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II, getBoardAndRegister}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue}
import Helper._


class Test28 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("America bonus") {
    val gg = getBoardAndRegister("test28/GAMEBOARD1.json", Civilization.America)
    val g: GameBoard = gg._2
    val token: String = gg._1
    val pl: PlayerLimits = getLimitsH(g, Civilization.America)
    println(pl.tradeforProd)
    assert(pl.tradeforProd == 3)
    assert(pl.prodfortrade == 2)
    val s = II.getData(II.GETBOARDGAME, token)
    val ju: JsValue = Helper.jyou(toJ(s))
    println(ju)
    val prodfortrade = (ju \ "prodfortrade").as[Int]
    println(prodfortrade)
    assert(prodfortrade == 2)
  }

  test("Generate board for America") {
    val token: String = II.getData(II.REGISTEROWNER, "America")
    println(token)
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.America)
    println(pl.cultureresource.persons.length)
    // great person at the beginning
    assert(pl.cultureresource.persons.length == 1)
  }

  test("Spend trade ratio for America") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD1.json", "test28/PLAY1.json", Civilization.America)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val prod = getProductionForCity(gg, Civilization.America, P(2, 2))
    println(prod)
    assert(prod.prod == 7)
    // increased by 2
    assert(prod.fromtrade == 2)
  }

  test("Masonry technology") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD2.json", "test28/PLAY2.json", Civilization.America)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val pl: PlayerLimits = getLimitsH(gg, Civilization.America)
    println(pl.handsize)
    // Masonry defaultHandSize
    assert(pl.handsize == 2)
    // Masonry stackLimit 3
    assert(pl.stackinglimit == 3)
    println(getProductionForCity(gg, Civilization.America, P(2, 2)))
    // allowed commands
    var l = allowedCommandsH(gg, Civilization.America)
    println(l)
    // in a single city
    assert(l contains Command.BUYCITYWALL)
    val ss = II.itemizeCommand(token, "BUYCITYWALL")
    println(ss)
    val ja = toJ(ss).as[JsArray]
    // walls in a single city
    assert(ja.value.length == 1)
    Helper.executeCommandH(token, "BUYCITYWALL", 5, 1)
    val bb = II.getData(II.GETBOARDGAME, token)
    val ma = Helper.jmap(toJ(bb))
    println(ma)
    var found = false
    ma.value.foreach(t => {
      val ta: JsArray = t.as[JsArray]
      ta.value.foreach(m => {
        println(m)
        val city = (m \ "city").asOpt[String]
        if (city.isDefined) {
          println(city.get)
          val defence = (m \ "defence").as[Int]
          println(defence)
          if (city.get == "WalledNormal") {
            assert(defence == 10)
            found = true
          }
        }
      })
    }
    )
    assert(found)
  }

  test("City wall for China in capital") {
    val token: String = II.getData(II.REGISTEROWNER, "China")
    println(token)
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    val ss = II.itemizeCommand(token, "SETCAPITAL")
    println(ss)
    Helper.executeCommandH(token, "SETCAPITAL", 1, 2)
    // China, capital walled immediately
    gg = I.getBoardForToken(token)
    val city = getSquare(gg, P(1, 2))
    println(city.s.city.get)
    assert(city.s.city.get.citytype == City.WalledCapital)
  }

  test("China takes Hut ") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD3.json", "test28/PLAY3.json", Civilization.China)
    val token = reg._1
    //    {"command":"EXPLOREHUT","civ":"China","p":{"row":4,"col":2},"param":null},
    Helper.executeCommandH(token, "EXPLOREHUT", 4, 2)
    var gg = I.getBoardForToken(token)
    val culture = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(culture)
    assert(culture == 3)
  }

  test("China, battle size of battle hand ") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD4.json", "test28/PLAY4.json", Civilization.China)
    val token = reg._1
    Helper.executeCommandH(token, "ATTACK", 2, 6)
    var gg = I.getBoardForToken(token)
    var b = gg.battle
    assert(b.isDefined)
    println(b.get.attacker.waiting.length)
    // 3 attackers
    assert(b.get.attacker.waiting.length == 3)
  }

  test("China, end of battle, lost ") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD5.json", "test28/PLAY5.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var b = gg.battle
    assert(b.isDefined)
    assert(b.get.endofbattle)
    assert(!b.get.attackerwinner)
    val culture = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(culture)
    Helper.executeCommandH(token, "ENDBATTLE", -1, -1)
    gg = I.getBoardForToken(token)
    val culture1 = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(culture1)
    // battle lost, no culture increase
    assert(culture == culture1)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l contains Command.STARTMOVE)
    assert(l contains Command.ENDOFPHASE)
  }

  test("China, win battle ") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD6.json", "test28/PLAY6.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val culture = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(culture)

    var bo = Helper.getB(token)
    var ba = Helper.getBattle(bo)
    var atta = (ba \ "attacker")
    println(atta)
    var cansave = (atta \ "saveunit").as[Boolean]
    println(cansave)
    assert(cansave)
    Helper.executeCommandH(token, "SAVEUNIT", 0, -1)

    bo = Helper.getB(token)
    ba = Helper.getBattle(bo)
    atta = (ba \ "attacker")
    cansave = (atta \ "saveunit").as[Boolean]
    println(cansave)
    assert(!cansave)

    gg = I.getBoardForToken(token)
    println(gg.battle.get.attacker.savedunit)
    assert(gg.battle.get.attacker.savedunit.isDefined)
    Helper.executeCommandH(token, "ENDBATTLE", -1, -1)
    gg = I.getBoardForToken(token)
    val culture1 = gg.playerDeck(Civilization.China).resou.nof(Resource.Culture)
    println(culture1)
    assert(culture1 == culture + 3)
    var l = allowedCommandsH(gg, Civilization.China)
    println(l)
    assert(l contains Command.STARTMOVE)
    assert(l contains Command.ENDOFPHASE)
    assert(!(l contains Command.MOVE))
    assert(!(l contains Command.ENDOFMOVE))
    println(gg.playerDeck(Civilization.China).units.length)
    // 5 units, not 4, one is saved
    assert(gg.playerDeck(Civilization.China).units.length == 5)
  }

  test("China, lost battle, save unit ") {
    val reg = Helper.readBoardAndPlayT("test28/GAMEBOARD7.json", "test28/PLAY7.json", Civilization.China)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    val punits = gg.playerDeck(Civilization.China).units.length
    println(punits)
    assert(punits == 1)
    val kunits = gg.market.killedunits.length
    println(kunits)
    assert(kunits == 5)
  }
}
