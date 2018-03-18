package civilization.test

  import civilization.I
  import civilization.I.II
  import civilization.gameboard.GameBoard
  import civilization.gameboard._
  import civilization.helper.AllowedCommands.allowedCommands
  import civilization.helper._
  import civilization.io.fromjson.ImplicitMiximFromJson
  import civilization.io.readdir._
  import civilization.objects._
  import org.scalatest.FunSuite
  import play.api.libs.json._
  import civilization.io.fromjson.{toJ, _}
  import civilization.io.readdir.GenBoard.genBoard
  import civilization.test.Helper._



  class Test22 extends FunSuite with ImplicitMiximFromJson {

    Helper.I

  test("Wonders") {
    val token: String = II.getData(II.REGISTEROWNER, "China")
    println(token)
    var gg: GameBoard = I.getBoardForToken(token)
    println(gg.market.wonders)
    assert(12 == gg.market.wonders.length)
    assert(4 == gg.market.wonders.filter(w => GameResources.getWonder(w).age == WondersAge.Modern).length)
  }

  test("Upgrade building") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME1.json", "test22/PLAY1.json", Civilization.Arabs)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val p  = getSquare(gg,P(2,1))
    println(p)
    // should be upgrade to IronMine
    assert(p.s.building.get.name == BuildingName.IronMine)
  }

  test("Research wonder") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME2.json", "test22/PLAY2.json", Civilization.Germany)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val ss = II.getData(II.GETBOARDGAME, token)
    println(ss)
    // should work without exception
  }

  test("Buy wonder") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME2.json", "test22/PLAY3.json", Civilization.Germany)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.BUYWONDER).isDefined)
    val ss = II.itemizeCommand(token,"BUYWONDER")
    println(ss)
    // buy wonder
    val c =
      """{"p":{"row":1,"col":1},"wonder":"Stonehenge"}"""
    Helper.executeCommandH(token, "BUYWONDER", 2, 2, c)
    gg = I.getBoardForToken(token)
    val p : MapSquareP = getSquare(gg,P(1,1))
    println(p)
    assert(p.s.wonder.get.w == Wonders.Stonehenge)
    // test wonders
    println(gg.market.wonders)
    // Stonehege should dissapear
    assert(gg.getCurrentWonders().find(_ == Wonders.Stonehenge).isEmpty)
  }

  test("Buy several times") {
    assertThrows[Exception] {
      Helper.readBoardAndPlayT("test22/BOARDGAME4.json", "test22/PLAY4.json", Civilization.Germany)
    }
  }

  test("Buy several times two players") {
    assertThrows[Exception] {
      val reg = Helper.ReadAndPlayForTwo("test22/BOARDGAME5.json", "test22/PLAY5.json", Civilization.Rome, Civilization.Russia)
    }
  }

    test("Check visible wonders") {
      val reg = Helper.readBoardAndPlayT("test22/BOARDGAME1.json", "test22/PLAY1.json", Civilization.Arabs)
      val token: String = reg._1
      val b = II.getData(II.GETBOARDGAME,token)
      val j:JsValue = toJ(b)
      println(j)
      val w : JsArray = (j \ "board" \ "wonders").as[JsArray]
      println(w)
      println(w.value.length)
      assert(w.value.length == 4)
    }

  test("Can affort two wonders") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME6.json", "test22/PLAY6.json", Civilization.Spain)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val mab: MapSquareP = getSquare(gg, P(2, 2))
    println(mab)
    var l = allowedCommands(gg, Civilization.Spain)
    println(l)
    assert(l.find(_ == Command.BUYWONDER).isDefined)
    val ss = II.getData(II.GETBOARDGAME, token)
    val jb = toJ(ss)
    val jy = jyou(jb)
    println(jy)
    val b = getProductionForCity(gg,Civilization.Spain,P(2,2))
    println(b.prod)
    assert(b.prod == 11)

    val ii = toJ(II.itemizeCommand(token,"BUYWONDER")).as[JsArray]
    println(ii)
    val pp = ii.value(0)
    println(pp)
    val li = (pp \ "list").as[JsArray]
    println(li)
    var isStonehege:Boolean = false
    var isPyramids : Boolean = false
    var isOracle : Boolean = false
    li.value.foreach(ee => {
      println(ee)
      val w = (ee \ "wonder").as[String]
      println(w)
      if (w == "Stonehenge") isStonehege = true
      if (w == "ThePyramids") isPyramids = true
      if (w == "TheOracle") isOracle = true
    }
    )
    assert(isStonehege)
    assert(isPyramids)
    assert(isOracle)
  }

  test("Buy wonder, check map") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME6.json", "test22/PLAY6.json", Civilization.Spain)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    val c =
      """{"p":{"row":3,"col":3},"wonder":"Stonehenge"}"""
    Helper.executeCommandH(token, "BUYWONDER", 2, 2, c)
    val ss = II.getData(II.GETBOARDGAME, token)
    val jm = jmap(toJ(ss))
    var buildno = 0
    var wonderno = 0
    jm.value.foreach(t => {
      val ta : JsArray = t.as[JsArray]
      ta.value.foreach(m => {
//        println(m)
        val b = (m \ "building").asOpt[String]
        if (b.isDefined) {
          buildno = buildno + 1
          println(b.get)
        }
        val w = (m \ "wonder").asOpt[String]
        if (w.isDefined) {
          wonderno = wonderno + 1
          println(w.get)
        }
      }
      )
    }
    )
    assert(buildno == 4)
    println(wonderno)
    assert(wonderno == 1)
  }

  test("Buy wonder, endofpahes") {
    val reg = Helper.readBoardAndPlayT("test22/BOARDGAME6.json", "test22/PLAY6.json", Civilization.Spain)
    val token: String = reg._1
    var gg: GameBoard = I.getBoardForToken(token)
    var b = getProductionForCity(gg,Civilization.Spain,P(2,2))
    println(b.prod)
    assert(b.prod == 11)
    val c =
      """{"p":{"row":3,"col":3},"wonder":"Stonehenge"}"""
    Helper.executeCommandH(token, "BUYWONDER", 2, 2, c)
    gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Spain)
    println(l)
    assert(l.find(_ == Command.BUYWONDER).isEmpty)
    assert(l.find(_ == Command.BUYBUILDING).isEmpty)
    // check production  for city
    b = getProductionForCity(gg,Civilization.Spain,P(2,2))
    // wonder, 0 production from square
    println(b.prod)
    assert(b.prod == 10)

    // check wonders for player
    val ss = II.getData(II.GETBOARDGAME, token)
    val jb = toJ(ss)
    val jy = jyou(jb)
    println(jy)
    val wA : JsArray = (jy \ "wonders").as[JsArray]
    println(wA)
    val s = wA.value(0).as[String]
    println(s)
    assert(s == "Stonehenge")
  }

  test("Check game, research") {
    val reg = Helper.ReadAndPlayForTwo("test22/BOARDGAME7.json", "test22/PLAY7.json", Civilization.Rome, Civilization.Russia)
    val token: String = reg._1
    // should fail ENDOFPHASE Research for the second time
    Helper.executeCommandFail(token, "ENDOFPHASE", -1, -1, "\"Research\"")
  }

  test("Check wonder not implemented") {
    val w : WondersOfTheWorld =GameResources.getWonder(Wonders.Stonehenge)
    println(w.notimplemented)
    assert(w.ni)
  }

  test("Test wonders as resource") {
    println("Test wonders")
    val s: String = II.getData(II.LISTOFRES)
    val j: JsValue = toJ(s)
    val w : JsArray = (j \ "wonders").as[JsArray]
    println(w)
    var wasS = false
    w.value.foreach( w => {
      println(w)
      val n = (w \ "name").as[String]
      if (n == "Stonehenge") {
        wasS = true
        val ni = (w \ "ni").as[Boolean]
        println("Stonehenge, check not implemented")
        assert(ni)
      }
    })
    assert(wasS)
  }

  test("Test technologies as resource") {
    println("Test technologies")
    val s: String = II.getData(II.LISTOFRES)
    val j: JsValue = toJ(s)
    val w : JsArray = (j \ "tech").as[JsArray]
//    println(w)
    var wasC = false
    w.value.foreach( w => {
      println(w)
      val n = (w \ "name").as[String]
      if (n == "RailRoad") {
        val co = (w \ "coin").as[Int]
        println(co)
        wasC = co == 1
      }
    })
    assert(wasC)
  }


  }
