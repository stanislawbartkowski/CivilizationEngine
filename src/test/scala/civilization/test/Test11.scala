package civilization.test

import civilization.I.{CurrentGame, WaitingGames, executeCommand}
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands
import civilization.helper.AllowedCommands.allowedCommands
import civilization.objects._
import civilization.{I, II, RR}
import org.scalatest.FunSuite
import play.api.libs.json.{JsArray, JsValue}
import civilization.io.fromjson.toJ


class Test11 extends FunSuite {

  Helper.I

  test("Two players game") {
    val token : String = II.getData(II.REGISTEROWNERTWOGAME,"Rome,China")
    println(token)
    val s = II.getData(II.GETBOARDGAME,token)
    println(s)
    val j = toJ(s)
    println(j)
    val ma:JsArray = (j \ "board" \ "map").as[JsArray]
    println(ma)
    var rome : Boolean = false
    var china : Boolean = false;
    ma.value.foreach(p => {
      val a : JsArray = p.as[JsArray];
      a.value.foreach( e => {
//        println(e)
        val civ : Option[String] = (e \ "capciv").asOpt[String]
        civ match  {
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
    var g : GameBoard = I.getBoardForToken(tokenr)
    var a : Seq[Command.T] = AllowedCommands.allowedCommands(g, Civilization.Rome)
    println(a)
    assert(a.contains(Command.BUYARMY))
    assert(a.contains(Command.BUYSCOUT))
    assert(a.contains(Command.ENDOFPHASE))
    a = AllowedCommands.allowedCommands(g, Civilization.China)
    println(a)
    assert(a.isEmpty)
    var s = executeCommand(tokenr, "ENDOFPHASE", -1, -1, "\"CityManagement\"")
    // now Rome is blocked
    assert (s == null)
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
    assert (s == null)
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
}


