package civilization.test

import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.test.Helper._
import civilization.io.fromjson.toJ
import play.api.libs.json.JsArray


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
    val glist : JsArray = toJ(II.getData(II.LISTOFGAMES)).as[JsArray]
    glist.value.foreach( e => {
      val g = (e \ "gameid").as[Int]
      println(g)
      assert(g != gameid)
    })
  }

}
