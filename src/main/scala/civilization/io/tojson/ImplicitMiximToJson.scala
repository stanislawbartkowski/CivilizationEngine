package civilization.io.tojson

import civilization.objects.P
import play.api.libs.json.{JsArray, JsValue, Json}

trait ImplicitMiximToJson {

  implicit def writesP(p: P): JsValue = Json.toJson(p)

  implicit def toSeqP(li : Seq[P]) : JsArray = {
    var l: Seq[JsValue] = li.map(p => writesP(p))
    JsArray(l)
  }

}
