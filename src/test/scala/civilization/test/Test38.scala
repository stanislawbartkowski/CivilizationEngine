package civilization.test

import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.test.Helper._
import civilization.io.fromjson.toJ
import civilization.message.FatalError
import play.api.libs.json.{JsArray, JsObject, JsValue}


class Test38 extends FunSuite with ImplicitMiximToJson with ImplicitMiximFromJson {

  Helper.I

  test("Create and remove game") {
    val reg = Helper.ReadAndPlayForTwo("test37/BOARDGAME1.json", "test37/PLAY1.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    val gameid = reg._3
    println(gameid)
    // remove game
    II.deleteGame(gameid)
    // check that game is removed
    val glist: JsArray = toJ(II.getData(II.LISTOFGAMES)).as[JsArray]
    glist.value.foreach(e => {
      val g = (e \ "gameid").as[Int]
      println(g)
      assert(g != gameid)
    })
  }

  test("Download game") {
    val reg = Helper.ReadAndPlayForTwo("test37/BOARDGAME1.json", "test37/PLAY1.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    val gameid = reg._3
    val g: String = II.downloadGame(gameid)
    println(g)
    II.deleteGame(gameid)
    // try to register games again
    //    val r = II.readTwoPlayerGameS(g._1,g._2,"America","China")
    val r = II.readPlayerGameS(g, "America,China")
    println(r)
    val nextgameid: Int = r.split(",")(2).toInt
    println(nextgameid)
    // remove
    II.deleteGame(nextgameid)
  }

  test("Resume nonexisting game") {
    val s: String = II.getData(II.LISTOFGAMES);
    println(s);
    val lgames: JsArray = toJ(s).as[JsArray]
    val maxid = lgames.value.map(s => {
      val j: JsObject = s.as[JsObject]
      println(j)
      val gameid: Int = j.value.get("gameid").get
      gameid
    }).toList.max
    assertThrows[FatalError] {
      val token: String = II.resumeGame(maxid + 1, "China")
      println(token)
    }
  }

}
