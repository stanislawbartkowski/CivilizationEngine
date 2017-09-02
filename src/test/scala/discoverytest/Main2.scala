package discoverytest

import scala.io.Source
import civilization.io.readdir._
import play.api.libs.json.JsValue

object Main2 {

  def main(args: Array[String]): Unit = {

    println("Hello")
    val l: List[(String, JsValue)] = readdirJSON("test/map/tiles")

    l.foreach(println)
  }
}
