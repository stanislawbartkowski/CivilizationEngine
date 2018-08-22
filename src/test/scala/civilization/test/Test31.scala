package civilization.test

import civilization.I
import civilization.gameboard.GameBoard
import civilization.helper._
import civilization.io.fromjson.{toJ, _}
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.{Civilization, Command, _}
import civilization.test.Helper.{II}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}
import Helper._


class Test31 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Russia start bonus") {
    val token: String = II.getData(II.REGISTEROWNER, "Russia")
    println(token)
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Russia)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "SETCAPITAL")
    println(ite)
    Helper.executeCommandH(token, "SETCAPITAL", 2, 2)

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Russia)
    println(l)
    ite = II.getData(II.ITEMIZECOMMAND, token, "SETARMY")
    println(ite)
    Helper.executeCommandH(token, "SETARMY", 2, 2, "{\"col\" : 2, \"row\" : 1}")

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Russia)
    println(l)
    // set army twice
    assert(l contains Command.SETARMY)
    Helper.executeCommandH(token, "SETARMY", 2, 2, "{\"col\" : 3, \"row\" : 3}")

    // again check
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Russia)
    println(l)
    // army limit exceeded
    assert(!(l contains Command.SETARMY))
    val li = getLimitsH(gg, Civilization.Russia)
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
    var l = allowedCommandsH(gg, Civilization.Russia)
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
    l = allowedCommandsH(gg, Civilization.Russia)
    println(l)
    // cannot sacrifice more figures
    assert(!(l contains Command.SACRIFICEFIGUREFORTECH))
  }

  test("Spain start bonus") {
    val token: String = II.getData(II.REGISTEROWNER, "Spain")
    println(token)
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "SETCAPITAL")
    println(ite)
    Helper.executeCommandH(token, "SETCAPITAL", 2, 2)

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    ite = II.getData(II.ITEMIZECOMMAND, token, "SETARMY")
    println(ite)
    Helper.executeCommandH(token, "SETARMY", 2, 2, "{\"col\" : 2, \"row\" : 1}")

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(!(l contains Command.SETARMY))
    assert(l contains Command.SETSCOUT)
    Helper.executeCommandH(token, "SETSCOUT", 2, 2, "{\"col\" : 2, \"row\" : 1}")

    // scout again
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(!(l contains Command.SETARMY))
    assert(l contains Command.SETSCOUT)
    Helper.executeCommandH(token, "SETSCOUT", 2, 2, "{\"col\" : 3, \"row\" : 3}")
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    // no more scouts and armies
    assert(!(l contains Command.SETARMY))
    assert(!(l contains Command.SETSCOUT))

    // check travel speed
    val li = getLimitsH(gg, Civilization.Spain)
    println(li.travelSpeed)
    assert(li.travelSpeed == 3)
  }

  test("Spain, free builiding after revealing the tile") {
    val reg = Helper.readBoardAndPlayT("test31/BOARDGAME2.json", "test31/PLAY2.json", Civilization.Spain)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(l contains Command.FREENONUPGRADEDBUILDING)
    var ite = II.getData(II.ITEMIZECOMMAND, token, "FREENONUPGRADEDBUILDING")
    val jite = toJ(ite).as[JsArray]
    println(jite)
    val ele: JsValue = jite.value.head
    println(ele)
    val li: JsArray = (ele \ "list").as[JsArray]
    println(li)
    var cathe: Boolean = false
    // check if not upgraded
    li.value.foreach(e => {
      val b = (e \ "building").as[String]
      println(b)
      if (b == "Cathedral") cathe = true
    })
    assert(!cathe)

    val c =
      """{"p":{"row":1,"col":1},"building":"Market"}"""
    Helper.executeCommandH(token, "FREENONUPGRADEDBUILDING", 2, 2, c)
    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(!(l contains Command.FREENONUPGRADEDBUILDING))

    val s = getSquare(gg, P(1, 1))
    println(s)
    assert(s.s.building.get.name == BuildingName.Market)

    ite = II.getData(II.ITEMIZECOMMAND, token, "REVEALTILE")
    println(ite)
    // reveal
    Helper.executeCommandH(token, "REVEALTILE", 1, 0, """ "Down" """)

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(l contains Command.FREENONUPGRADEDBUILDING)
  }

  test("Spain, free builiding after revealing the tile again") {
    val reg = Helper.readBoardAndPlayT("test31/BOARDGAME2.json", "test31/PLAY3.json", Civilization.Spain)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Spain)
    println(l)
    assert(l contains Command.FREENONUPGRADEDBUILDING)
  }

  test("Arabs start bonus, free resources") {
    val token: String = II.getData(II.REGISTEROWNER, "Arabs")
    println(token)
    var gg = I.getBoardForToken(token)
    val deck = gg.playerDeck(Civilization.Arabs)
    println(deck.resou.table)
    assert(deck.resou.nof(Resource.Iron) == 1)
    assert(deck.resou.nof(Resource.Silk) == 1)
    assert(deck.resou.nof(Resource.Incense) == 1)
    assert(deck.resou.nof(Resource.Wheat) == 1)
    // removed from market
    assert(gg.resources.resou.nof(Resource.Iron) == 0)
  }

  test("Arabs, spend resources") {
    val reg = Helper.readBoardAndPlayT("test31/BOARDGAME4.json", "test31/PLAY4.json", Civilization.Arabs)
    val token = reg._1
    var gg = I.getBoardForToken(token)
    var l = allowedCommandsH(gg, Civilization.Arabs)
    println(l)
    assert(l contains Command.POTTERYACTION)
    val pa = """ [{"resource":"Incense"},{"resource":"Iron"}] """
    Helper.executeCommandH(token, "POTTERYACTION", 2, 2, pa)
    gg = I.getBoardForToken(token)
    val cu = gg.playerDeck(Civilization.Arabs).resou.nof(Resource.Culture)
    println(cu)
    assert(cu == 2)
  }

  test("Arabs start bonus, verify military level") {
    val token: String = II.getData(II.REGISTEROWNER, "Arabs")
    println(token)
    var gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Arabs)
    println(pl.tech)
    assert(pl.tech.length == 1)
    val t = pl.tech.head
    println(t)
    assert(t.initial.isDefined)
    val s = pl.combatlevel.getStrength(CombatUnitType.Artillery)
    // artillery strength = 1
    assert(s == 1)

    // check civ in board
    val b = getB(token)
    val boa = (b \ "board" \ "you").as[JsValue]
    println(boa)
    val civ = (boa \ "civ").as[String]
    // should be string
    assert(civ == "Arabs")
    //    println(b)

    val o = (b \ "board" \ "others").as[JsArray]
    println(o)
    println(o.value.length)
    assert(o.value.isEmpty)
  }

  test("Technology HorseBackRiding") {
    val reg = Helper.readBoardAndPlayT("test31/BOARDGAME5.json", "test31/PLAY5.json", Civilization.Arabs)
    val token = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val limi = getLimitsH(gg, Civilization.Arabs)
    println(limi)
    println(limi.travelSpeed)
    // HorseBack Riding, speed 3
    assert(limi.travelSpeed == 3)
    var l = allowedCommandsH(gg, Civilization.Arabs)
    println(l)
    assert(l contains Command.USESILKFORTRADE9)
    val cu = currentPhase(gg)
    println(cu)
    val tra1 = numberofTradeH(gg, Civilization.Arabs)
    println(tra1.trade)
    val ite = II.getData(II.ITEMIZECOMMAND, token, "USESILKFORTRADE9")
    println(ite)
    val pa =
      """
        {
          "resource" : {
            "hv" : null,
            "resource" : "Silk"
          },
          "civ" : null
        }
      """
    Helper.executeCommandH(token, "USESILKFORTRADE9", -1, -1, pa)

    gg = I.getBoardForToken(token)
    l = allowedCommandsH(gg, Civilization.Arabs)
    println(l)
    assert(!(l contains Command.USESILKFORTRADE9))
    val tra2 = numberofTradeH(gg, Civilization.Arabs)
    println(tra2.trade)
    val numsilka = gg.playerDeck(Civilization.Arabs).resou.nof(Resource.Silk)
    println(numsilka)
    assert(numsilka == 0)

    val numcult = gg.playerDeck(Civilization.Arabs).resou.nof(Resource.Culture)
    println(numcult)
    assert(numcult == 1)

    // increase culture for Arabs


    // resource on the market
    val numsilk = gg.resources.resou.nof(Resource.Silk)
    println(numsilk)
    // no silk
    assert(numsilk == 1)
    val nu = numberofTradeCalculateH(gg, Civilization.Arabs)
    println(nu)
//    assert(nu.increasebysilk == 9)
    assert(nu.trade == 5)
    val cu1 = currentPhase(gg)
    println(cu1)
    Helper.executeCommandH(token, "ENDOFPHASE", -1, -1, """ "Trade" """)
    gg = I.getBoardForToken(token)
    val tra3 = numberofTradeH(gg, Civilization.Arabs)
    println(tra3)
    assert(tra3.trade == 14)
  }

  test("Silk for twa players") {
    val reg = Helper.ReadAndPlayForTwo("test31/BOARDGAME6.json", "test31/PLAY6.json", Civilization.America, Civilization.Arabs)
    val tokenAm = reg._1
    val tokenAr = reg._2
    var gg: GameBoard = I.getBoardForToken(tokenAm)
    val tra1 = numberofTradeH(gg, Civilization.Arabs)
    println(tra1.trade)
    val tra2 = numberofTradeH(gg, Civilization.America)
    println(tra2.trade)
    var l = allowedCommandsH(gg, Civilization.Arabs)
    println(l)
    val ite = II.getData(II.ITEMIZECOMMAND, tokenAr, "USESILKFORTRADE9")
    println(ite)
    val j: JsArray = toJ(ite).as[JsArray]
    println(j.value.length)
    assert(j.value.length == 1)
    var la = allowedCommandsH(gg, Civilization.America)
    println(la)
    assert(!(la contains Command.USESILKFORTRADE9))

    val pa =
      """
        {
          "resource" : {
            "hv" : null,
            "resource" : "Silk"
          },
          "civ" : "America"
        }
      """
    Helper.executeCommandH(tokenAr, "USESILKFORTRADE9", -1, -1, pa)
    Helper.executeCommandH(tokenAr, "ENDOFPHASE", -1, -1, """ "Trade" """)
    Helper.executeCommandH(tokenAm, "ENDOFPHASE", -1, -1, """ "Trade" """)
    gg = I.getBoardForToken(tokenAm)
    val tra3 = numberofTradeH(gg, Civilization.Arabs)
    println(tra3.trade)
    assert(tra3.trade == 14)
    val tra4 = numberofTradeH(gg, Civilization.America)
    println(tra4.trade)
    assert(tra4.trade == 10)
  }

  test("Silk for two players, different order") {
    val reg = Helper.ReadAndPlayForTwo("test31/BOARDGAME6.json", "test31/PLAY6.json", Civilization.America, Civilization.Arabs)
    val tokenAm = reg._1
    val tokenAr = reg._2
    // America ends Trade
    Helper.executeCommandH(tokenAm, "ENDOFPHASE", -1, -1, """ "Trade" """)
    // now Arabs play silk
    val pa =
      """
        {
          "resource" : {
            "hv" : null,
            "resource" : "Silk"
          },
          "civ" : "America"
        }
      """
    Helper.executeCommandH(tokenAr, "USESILKFORTRADE9", -1, -1, pa)
    Helper.executeCommandH(tokenAr, "ENDOFPHASE", -1, -1, """ "Trade" """)
    // take trade
    var gg: GameBoard = I.getBoardForToken(tokenAm)
    val tra1 = numberofTradeH(gg, Civilization.Arabs)
    println(tra1.trade)
    assert(tra1.trade == 14)
    val tra2 = numberofTradeH(gg, Civilization.America)
    println(tra2.trade)
    assert(tra2.trade == 10)
  }
}