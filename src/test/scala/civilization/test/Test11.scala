package civilization.test

import civilization.I._
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands
import civilization.io.fromjson.toJ
import civilization.objects._
import civilization.{I, II}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsString, JsValue}


class Test11 extends FunSuite {

  Helper.I

  test("Two players game") {
    val token: String = II.getData(II.REGISTEROWNERTWOGAME, "Rome,China")
    println(token)
    val s = II.getData(II.GETBOARDGAME, token)
    println(s)
    val j = toJ(s)
    println(j)
    val ma: JsArray = (j \ "board" \ "map").as[JsArray]
    println(ma)
    var rome: Boolean = false
    var china: Boolean = false;
    ma.value.foreach(p => {
      val a: JsArray = p.as[JsArray];
      a.value.foreach(e => {
        //        println(e)
        val civ: Option[String] = (e \ "capciv").asOpt[String]
        civ match {
          case Some(s) => {
            println(s)
            if (s == "Rome") rome = true
            if (s == "China") china = true
          }
          case _ => {}
        }
      }
      )
    })
    assert(rome)
    assert(china)
  }

  test("Two players games") {
    val c = Helper.ReadAndPlayForTwo("test10/BOARDGAME2.json", "test11/GAME1.json", Civilization.Rome, Civilization.China)
    val tokenr = c._1
    val tokenc = c._2
    var g: GameBoard = I.getBoardForToken(tokenr)
    var a: Seq[Command.T] = AllowedCommands.allowedCommands(g, Civilization.Rome)
    println(a)
    assert(a.contains(Command.BUYARMY))
    assert(a.contains(Command.BUYSCOUT))
    assert(a.contains(Command.ENDOFPHASE))
    a = AllowedCommands.allowedCommands(g, Civilization.China)
    println(a)
    assert(a.isEmpty)
    var s = executeCommand(tokenr, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now Rome is blocked
    assert(s == null)
    g = I.getBoardForToken(tokenr)
    a = AllowedCommands.allowedCommands(g, Civilization.Rome)
    println(a)
    assert(a.isEmpty)
    // china is unlocked
    a = AllowedCommands.allowedCommands(g, Civilization.China)
    println(a)
    assert(a.contains(Command.BUYARMY))
    assert(a.contains(Command.BUYSCOUT))
    assert(a.contains(Command.ENDOFPHASE))
    s = executeCommand(tokenc, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now Rome is blocked
    assert(s == null)
    // next phase
    g = I.getBoardForToken(tokenr)
    a = AllowedCommands.allowedCommands(g, Civilization.Rome)
    println(a)
    assert(a.contains(Command.STARTMOVE))
    assert(a.contains(Command.ENDOFPHASE))
    // China is blocked
    a = AllowedCommands.allowedCommands(g, Civilization.China)
    println(a)
    assert(a.isEmpty)
    s = executeCommand(tokenr, "ENDOFPHASE", -1, -1, "\"Movement\"")
    // Rome is blocked again
    g = I.getBoardForToken(tokenr)
    a = AllowedCommands.allowedCommands(g, Civilization.Rome)
    println(a)
    assert(a.isEmpty)
    // china is unlocked
    a = AllowedCommands.allowedCommands(g, Civilization.China)
    println(a)
    assert(a.contains(Command.STARTMOVE))
    assert(a.contains(Command.ENDOFPHASE))
  }

  test("List of civs") {
    val s: String = II.getData(II.LISTOFCIV, "")
    println(s)
    assert(s.contains("Rome"))
    assert(s.contains("Spain"))
  }

  test("hut villages exported") {
    var b: GameBoard = Helper.readBoardAndPlay("test9/BOARDGAME1.json", "test9/GAME8.json", Civilization.Rome)
    val token: String = registerGame(b, Civilization.Rome)
    var s: String = II.getData(II.GETBOARDGAME, token)
    println(s)
    val j: JsValue = toJ(s)
    println(j)
    val a = (j \ "board" \ "map").get.as[JsArray]
    println(a)
    var hut: Int = 0
    var village: Int = 0
    a.value.foreach(v => v.as[JsArray].value.foreach(s => {
      //println(s)
      val tile: String = (s \ "tile").get.as[String]
      assert(tile != null)
      val hv: Option[JsString] = (s \ "hv").get.asOpt[JsString]
      if (hv.isDefined) {
        val s: String = hv.get.as[String]
        println(hv.get)
        if (s == "Hut") hut = hut + 1
      }
      //
    }))
    assert(hut == 2)
    assert(village == 0)
  }

  test("cannot set city close to huts") {
    var b: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test11/GAME2.json", Civilization.Rome)
    val token: String = registerGame(b, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var a: Seq[Command.T] = AllowedCommands.allowedCommands(g, Civilization.Rome)
    assert(!a.contains(Command.SETCITY))
    println(a)
    val o: String = itemizeCommand(token, "SETCITY")
    println(o)
  }

  test("cannot exceed the hand size") {
    var b: GameBoard = Helper.readBoardAndPlay("test11/BOARDGAME1.json", "test11/GAME3.json", Civilization.Rome)
    val token: String = registerGame(b, Civilization.Rome)
    var g: GameBoard = I.getBoardForToken(token)
    var a: Seq[Command.T] = AllowedCommands.allowedCommands(g, Civilization.Rome)
    println(a)
    val o: String = itemizeCommand(token, "MOVE")
    println(o)
    val j: JsValue = toJ(o)
    println(j)
    val am = (j \ "moves").get.as[JsArray]
    println(am)
    val res: Option[JsValue] = am.value.toStream.find(p => {
      val row : Int = (p \ "row").as[Int]
      val col : Int = (p \ "col").as[Int]
      ( row == 3 && col == 1)
    }
    )
    assert(res.isEmpty)
  }

  }
