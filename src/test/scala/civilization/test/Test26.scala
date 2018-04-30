package civilization.test

import civilization.I
import civilization.I.II
import civilization.gameboard._
import civilization.gameboard.CultureTrack._
import civilization.helper.AllowedCommands.allowedCommands
import civilization.io.fromjson.ImplicitMiximFromJson
import civilization.io.tojson.ImplicitMiximToJson
import civilization.objects.Civilization
import org.scalatest.FunSuite
import civilization.helper._
import civilization.objects._
import play.api.libs.json.{JsArray, JsValue}
import civilization.io.fromjson._
import civilization.io.readdir.GameResources

class Test26 extends FunSuite with ImplicitMiximToJson {

  Helper.I

  test("Pottery only once per turn") {

    val reg = Helper.readBoardAndPlayT("test26/BOARDGAME1.json", "test26/PLAY1.json", Civilization.Egypt)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    var l = allowedCommands(gg, Civilization.Egypt)
    println(l)
    // pottery action already consumed
    assert(!(l contains Command.POTTERYACTION))
  }

  test("Iron in battle only in case of Barrack") {

    val reg = Helper.readBoardAndPlayT("test26/BOARDGAME2.json", "test26/PLAY2.json", Civilization.Germany)
    val token = reg._1
    // Germany: Metallurgy, initial technology
    Helper.verifyiron(token,true)
  }

  test("Iron in battle only in case of Barrack Rome") {

    val reg = Helper.readBoardAndPlayT("test26/BOARDGAME3.json", "test26/PLAY3.json", Civilization.Rome)
    val token = reg._1
    // cannot use iron
    Helper.verifyiron(token,false)
 }

  test("Iron in battle only in case of Barrack Rome cannot use for the second time") {

    val reg = Helper.readBoardAndPlayT("test26/BOARDGAME3.json", "test26/PLAY4.json", Civilization.Rome)
    val token = reg._1
    // cannot use iron
    Helper.verifyiron(token,false)
  }

  test("Use iron from village") {

    val reg = Helper.readBoardAndPlayT("test26/BOARDGAME5.json", "test26/PLAY5.json", Civilization.Germany)
    val token = reg._1
    // can user
    Helper.verifyiron(token,true)
  }

  test("Use iron from village and spend it") {

    val reg = Helper.readBoardAndPlayT("test26/BOARDGAME5.json", "test26/PLAY6.json", Civilization.Germany)
    val token = reg._1
    val gg = I.getBoardForToken(token)
    val pl = gg.playerDeck(Civilization.Germany)
    // hut with Iron is user
    assert(pl.hvlist.find(_.resource == Resource.Iron).isEmpty)
    // moved to used
    assert(gg.resources.hvused.find(_.resource == Resource.Iron).isDefined)
    // resource reported
    // still one Iron
    assert(gg.resources.resou.nof(Resource.Iron) == 1)
    val s = II.getData(II.GETBOARDGAME, token)
    //        println(s)
    val j: JsValue = toJ(s)
    val r = Helper.jresources(j)
    println(r)
    var nofiron : Int = 0
    r.value.foreach(r => {
      val reso = (r \ "resource").as[String]
      val num = (r \ "num").as[Int]
      if (reso == "Iron") nofiron = num
    }
    )
    println(nofiron)
    assert(nofiron == 1)
  }

  test("Iron invisible for opponent") {
    val reg = Helper.ReadAndPlayForTwo("test26/BOARDGAME7.json", "test26/PLAY7.json", Civilization.America, Civilization.China)
    val tokenA = reg._1
    val tokenC = reg._2
    // iron visible for China
    Helper.verifyiron(tokenC,true)
//    val s = II.getData(II.GETBOARDGAME, tokenA)
//    val js: JsValue = toJ(s)
//    val batt: JsValue = (js \ "board" \ "battle").get
//    println(batt)
    // from point of view America, the defender, iron should be invisible
    Helper.verifyiron(tokenA,false)




  }


}