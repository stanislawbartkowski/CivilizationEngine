package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard._
import civilization.helper._
import civilization.io.fromjson._
import civilization.io.tojson._
import civilization.io.readdir.GenBoard.genBoard
import civilization.objects.{Civilization, Command}
import org.scalatest.FunSuite
import play.api.libs.json._
import civilization.objects._
import Helper._

class Test17 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("Test explore hut") {
    val reg = Helper.readBoardAndPlayT("test17/BOARD1.json", "test17/GAME1.json", Civilization.America)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    var l: Seq[Command.T] = allowedCommandsH(g, Civilization.America)
    println(l)
    val i: String = II.itemizeCommand(token, "EXPLOREHUT")
    val j: JsValue = toJ(i)
    println(i)
    Helper.executeCommandH(token, "EXPLOREHUT", 6, 3, null)
    g = I.getBoardForToken(token)
    l = allowedCommandsH(g, Civilization.America)
    println(l)
    assert(l.find(_ == Command.MOVE).isEmpty)
    assert(l.find(_ == Command.REVEALTILE).isEmpty)
    assert(l.find(_ == Command.ENDOFMOVE).isEmpty)
    assert(l.find(_ == Command.ENDOFPHASE).isDefined)
    // check journal
    val s = II.getData(II.GETJOURNAL, token)
    println(s)
    val jo: JsArray = toJ(s).as[JsArray]
    println(jo)
    val je : JsValue = jo.value(1)
    println(je)
    val ele: JsValue = (je \ "elem").as[JsValue]
    println(ele)
    val a = (ele \ "jartifacts").as[JournalElem.JournalArtifacts]
    println(a)
    // resource
    assert(a.res.isDefined)
    assert(a.res.get == Resource.Silk)
  }

  test("Gen BoardGJ, check that resources for hut and villages are preserved") {

    val g: GameBoard = genBoard(List(Civilization.Germany), "TEST1.json")
    var noh: Int = 0
    g.map.map.foreach(
      m => m.mapsquares.foreach(mm => mm.foreach(
        ss => {
          println(ss)
          if (ss.hvhere && ss.hv.get.resource != null) noh = noh + 1
        }
      ))
    )
    println(noh)
    assert(noh > 0)
    val (token: String, gameid : Int) = civilization.I.decodeS(civilization.I.registerGame(g, Civilization.Germany))
    var gg: GameBoard = I.getBoardForToken(token)
    var nohh = 0
    gg.map.map.foreach(
      m => m.mapsquares.foreach(mm => mm.foreach(
        ss => {
          println(ss)
          if (ss.hvhere && ss.hv.get.resource != null) nohh = nohh + 1
        }
      ))
    )
    println(nohh)
    assert(noh == nohh)
    //    val bs: String = II.getData(II.GETBOARDGAME, token)
    //    println(bs)
  }

  test("Test Attack Village") {
    val reg = Helper.readBoardAndPlayT("test17/BOARD2.json", "test17/GAME2.json", Civilization.Rome)
    val token: String = reg._1
    var g: GameBoard = I.getBoardForToken(token)
    val l = allowedCommandsH(g, Civilization.Rome)
    println(l)
    assert(l.find(_ == Command.ATTACK).isDefined)
    val i: String = II.itemizeCommand(token, "ATTACK")
    val j: JsValue = toJ(i)
    println(i)
    val a: JsArray = (j \ "attack").as[JsArray]
    println(a)
    val p: P = a.value.head.as[P]
    assert(P(4, 0) == p)
    Helper.executeCommandH(token, "ATTACK", 4, 0, null)
    // all units in battle
    g = I.getBoardForToken(token)
    assert(g.market.units.isEmpty)
    val pl: PlayerDeck = g.playerDeck(Civilization.Rome)
    assert(pl.units.isEmpty)
    val com = g.play.commands.last
    val n: BattleStart = com.param.asInstanceOf[BattleStart]
    val jj: JsValue = n
    println(jj)
    val s = II.getData(II.GETBOARDGAME, token)
    println(s)
    val js: JsValue = toJ(s)
    val batt: JsValue = (js \ "board" \ "battle").get
    println(batt)
    assert(batt != null)
  }

  private def checkkilled(j: JsValue, side: String, expected: Int) = {
    val jj: JsArray = (j \ side \ "killedunits").get.as[JsArray]
    println(jj)
    assert(expected == jj.value.size)
  }

  private def checklist(j: JsValue, side: String, expected: Int) = {
    val jj: JsArray = (j \ side \ "waiting" \ "list").get.as[JsArray]
    println(jj)
    assert(expected == jj.value.size)
  }

  private def checkfront(j: JsValue, side: String, expected: Int) = {
    var no = 0
    val jj: JsArray = (j \ side \ "front").get.as[JsArray]
    println(jj)
    jj.value.foreach(p => if (p != JsNull) no = no + 1)
    assert(expected == no)
  }

  private def checkturn(j: JsValue, side: String, expected: Boolean): Unit = {
    val t: Boolean = (j \ side \ "turn").get.as[Boolean]
    println(t)
    assert(expected == t)
  }

  test("Test Attack Village and Start") {
    val reg = Helper.readBoardAndPlayT("test17/BOARD2.json", "test17/GAME3.json", Civilization.Rome)
    val token: String = reg._1
    // defender
    Helper.executeCommandH(token, "PLAYUNIT", 0, 0, null)
    var g: GameBoard = I.getBoardForToken(token)
//    println(g.battle)
    val b: BattleField = g.battle.get
    // attacker move
    assert(b.attackermove)
    assert(2 == b.defender.waiting.length)
    assert(b.defender.fighting(0).isDefined)
    var s = II.getData(II.GETBOARDGAME, token)
    var js: JsValue = toJ(s)
    var batt: JsValue = (js \ "board" \ "battle").get
    println(Json.prettyPrint(batt))
    checkkilled(batt, "attacker", 0)
    checkkilled(batt, "defender", 0)
    checkfront(batt, "attacker", 0)
    checkfront(batt, "defender", 1)
    checklist(batt, "attacker", 3)
    checklist(batt, "defender", 2)
    checkturn(batt, "attacker", true)
    checkturn(batt, "defender", false)
    Helper.checkendofbattle(batt, false)

    //attacker
    // Artillery again Infantry
    Helper.executeCommandH(token, "PLAYUNIT", 0, 0, null)
    s = II.getData(II.GETBOARDGAME, token)
    js = toJ(s)
    batt = (js \ "board" \ "battle").get
    println("=============================")
    println(Json.prettyPrint(batt))
    checkkilled(batt, "attacker", 0)
    checkkilled(batt, "defender", 1)
    checkfront(batt, "attacker", 1)
    checkfront(batt, "defender", 0)
    checklist(batt, "attacker", 2)
    checklist(batt, "defender", 2)
    checkturn(batt, "attacker", false)
    checkturn(batt, "defender", true)

    // defender
    // attack with mounted
    Helper.executeCommandH(token, "PLAYUNIT", 1, 0, null)
    s = II.getData(II.GETBOARDGAME, token)
    js = toJ(s)
    batt = (js \ "board" \ "battle").get
    println("=============================")
    println(Json.prettyPrint(batt))
    checkkilled(batt, "attacker", 0)
    checkkilled(batt, "defender", 2)
    checkfront(batt, "attacker", 1)
    checkfront(batt, "defender", 0)
    checklist(batt, "attacker", 2)
    checklist(batt, "defender", 1)
    checkturn(batt, "attacker", true)
    checkturn(batt, "defender", false)

    // attacker:
    // play mounted
    Helper.executeCommandH(token, "PLAYUNIT", 0, 1, null)
    s = II.getData(II.GETBOARDGAME, token)
    js = toJ(s)
    batt = (js \ "board" \ "battle").get
    println("=============================")
    println(Json.prettyPrint(batt))
    checkkilled(batt, "attacker", 0)
    checkkilled(batt, "defender", 2)
    checkfront(batt, "attacker", 2)
    checkfront(batt, "defender", 0)
    checklist(batt, "attacker", 1)
    checklist(batt, "defender", 1)
    checkturn(batt, "attacker", false)
    checkturn(batt, "defender", true)

    // defender:
    // attack with artlery, both killed
    Helper.executeCommandH(token, "PLAYUNIT", 0, 0, null)
    s = II.getData(II.GETBOARDGAME, token)
    js = toJ(s)
    batt = (js \ "board" \ "battle").get
    println("=============================")
    println(Json.prettyPrint(batt))
    checkkilled(batt, "attacker", 1)
    checkkilled(batt, "defender", 3)
    checkfront(batt, "attacker", 1)
    checkfront(batt, "defender", 0)
    checklist(batt, "attacker", 1)
    checklist(batt, "defender", 0)
    checkturn(batt, "attacker", true)
    checkturn(batt, "defender", false)

    // attacker: play last unit
    Helper.executeCommandH(token, "PLAYUNIT", 0, 0, null)
    s = II.getData(II.GETBOARDGAME, token)
    js = toJ(s)
    batt = (js \ "board" \ "battle").get
    println("=============================")
    println(Json.prettyPrint(batt))
    checkkilled(batt, "attacker", 1)
    checkkilled(batt, "defender", 3)
    checkfront(batt, "attacker", 2)
    checkfront(batt, "defender", 0)
    checklist(batt, "attacker", 0)
    checklist(batt, "defender", 0)
    checkturn(batt, "attacker", true)
    checkturn(batt, "defender", false)
    Helper.checkendofbattle(batt, true)
    Helper.checkattackerwinner(batt, true)

    // end of the battle
    Helper.executeCommandH(token, "ENDBATTLE", 0, 0, """ [] """)
    s = II.getData(II.GETBOARDGAME, token)
    js = toJ(s)
    println("=============================")
    println(Json.prettyPrint(js))
    // no active battle
    batt = (js \ "board" \ "battle").get
    assert(batt == JsNull)
    val gg: GameBoard = I.getBoardForToken(token)
    var ma: MapSquareP = getSquare(gg, P(3, 0))
    // no figures
    assert(ma.civHere.isEmpty)
    ma = getSquare(gg, P(4, 0))
    assert(ma.civHere.get == Civilization.Rome)
    assert(ma.s.figures.numberofArmies == 1)
    val pl: PlayerDeck = gg.playerDeck(Civilization.Rome)
    // village taken
    assert(pl.hvlist.length == 1)
    // two units, one killed
    assert(pl.units.length == 2)
    // killed units: 4 killed in the battle and 1 before
    assert(gg.market.killedunits.length == 5)

    // check available move
    val l = allowedCommandsH(gg, Civilization.Rome)
    println(l)
    assert(l.filter(_ == Command.MOVE).isEmpty)
    assert(l.filter(_ == Command.ENDOFMOVE).isEmpty)
    assert(!l.filter(_ == Command.ENDOFPHASE).isEmpty)
  }

}