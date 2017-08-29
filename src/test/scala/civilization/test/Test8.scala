package civilization.test

import civilization.I.{GETBOARDGAME, REGISTEROWNER, executeCommand, getBoardForToken, getData, itemizeCommand}
import civilization.gameboard.GameBoard
import org.scalatest.FunSuite
import civilization.helper._
import civilization.objects._
import civilization.message._
import civilization.helper.AllowedCommands.{allowedCommands, itemizeForSetBuyFigures}
import civilization.io.fromjson.toJ
import play.api.libs.json.JsValue
import play.api.libs.json._
import civilization.io.fromjson._

class Test8 extends FunSuite {

  Helper.I

  test("Execute command, check available") {
    val token: String = getData(REGISTEROWNER, "Germany")
    var g: GameBoard = getBoardForToken(token);
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(!l.isEmpty)
    assert(Command.SETCAPITAL == l.head)
    var s: String = executeCommand(token, "SETCAPITAL", 2, 2, null)
    println(s)
    assert(s == null)
    g = getBoardForToken(token);
    l = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.ENDOFPHASE).isDefined)
    s = executeCommand(token, "ENDOFPHASE", 2, 2, "\"StartOfTurn\"")
    println(s)
    assert(s == null)
    l = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.ENDOFPHASE).isDefined)
  }

  test("Execute command, set army and scout") {
    val token: String = getData(REGISTEROWNER, "Germany")
    var g: GameBoard = getBoardForToken(token);
    var s: String = executeCommand(token, "SETCAPITAL", 2, 2, null)
    g = getBoardForToken(token);
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.SETSCOUT).isDefined)
    assert(l.find(_ == Command.SETARMY).isDefined)
    s = executeCommand(token, "SETARMY", 2, 2, "{\"col\" : 1, \"row\" : 2}")
    println(s)
    assert(s == null)
    g = getBoardForToken(token);
    l = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.SETSCOUT).isDefined)
    assert(l.find(_ == Command.SETARMY).isEmpty)
    s = executeCommand(token, "SETSCOUT", 2, 2, "{\"col\" : 1, \"row\" : 1}")
    println(s)
    g = getBoardForToken(token);
    l = allowedCommands(g, Civilization.Germany)
    println(l)
    assert(l.find(_ == Command.SETSCOUT).isEmpty)
    assert(l.find(_ == Command.SETARMY).isEmpty)
  }

  test("Execute command, itemized") {
    val token: String = getData(REGISTEROWNER, "Rome")
    var g: GameBoard = getBoardForToken(token);
    var s: String = executeCommand(token, "SETCAPITAL", 1, 2, null)
    println(s)
    assert(s == null)
    g = getBoardForToken(token);
    var l: Seq[Command.T] = allowedCommands(g, Civilization.Rome)
    println(l)
    var a: Seq[(P, P)] = itemizeForSetBuyFigures(g, Civilization.Rome, Command.SETARMY)
    println(a)
    assert(a.length == 6)
    assert(a.find(_._2 == P(0, 3)).isEmpty)
    assert(a.find(_._2 == P(1, 3)).isEmpty)
    assert(a.find(_._2 == P(2, 3)).isDefined)
    // check all
    assert(a.find(_._2 == P(0, 1)).isDefined)
    assert(a.find(_._2 == P(1, 1)).isDefined)
    assert(a.find(_._2 == P(2, 1)).isDefined)
    assert(a.find(_._2 == P(0, 2)).isDefined)
    assert(a.find(_._2 == P(2, 2)).isDefined)
    assert(a.find(_._2 == P(2, 3)).isDefined)

    val o: String = itemizeCommand(token, "SETSCOUT")
    // double check
    println(o)
    var p : Set[P] = a.map(_._2).toSet
    assert(p.size == 6)
    val re : JsArray = toJ(o).as[JsArray]
    re.value.foreach(e => {
      println(e)
      val reco:P = toP((e \ "param").get.as[JsValue])
      println(reco)
      p = p - reco
    })
    assert(p.isEmpty)
  }

  test("Check production for city") {
    val token: String = getData(REGISTEROWNER, "Rome")
    var g: GameBoard = getBoardForToken(token);
    var s: String = executeCommand(token, "SETCAPITAL", 1, 2, null)
    println(s)
    assert(s == null)
    s = getData(GETBOARDGAME,token)
//    println(s)
    val board : JsValue = toJ(s)
    val map : JsArray = (board \ "board" \ "map").get.as[JsArray]
//    println (map)
    var found : Int = 0
    map.value.foreach(v => v.as[JsArray].value.foreach(s => {
       //println(s)
       val capciv : Option[JsString] = (s \ "civ").get.asOpt[JsString]
       if (capciv.isDefined) {
         println(capciv.get)
         found = found+1
         val production = (s \ "production").get.as[Int]
         println(production)
         assert(production == 6)
       }
//       println(capciv)
    }))
    assert(found == 1)
  }

}
