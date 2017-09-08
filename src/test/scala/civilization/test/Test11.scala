package civilization.test

import civilization.I.{CurrentGame, WaitingGames, executeCommand}
import civilization.gameboard.GameBoard
import civilization.helper.AllowedCommands.allowedCommands
import civilization.objects._
import civilization.{I, II, RR}
import org.scalatest.FunSuite
import org.scalatest.Matchers._
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
}


