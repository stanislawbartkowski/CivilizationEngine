package discoverytest

import civilization.io.readdir.readTestJSON
import play.api.libs.json.{JsPath, JsValue, Json, JsDefined}

object Main4 {

  def main(args: Array[String]): Unit = {

    val l: JsValue = readTestJSON("resources/map/tiles/TILE1.json")

    println(l)

    l.productIterator.foreach(p => println({
      val d: JsDefined = p.asInstanceOf[JsDefined];
    }))

  }

}
