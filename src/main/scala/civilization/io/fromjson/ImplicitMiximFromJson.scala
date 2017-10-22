package civilization.io.fromjson

import civilization.objects.{P, CommandParams}
import play.api.libs.json.{JsArray, JsValue, Json}

trait ImplicitMiximFromJson {

  implicit def toPoint(j: JsValue): P = convert[PJ](PJ(j))

  implicit def toInt(j : JsValue) : Int = j.as[Int]

}
