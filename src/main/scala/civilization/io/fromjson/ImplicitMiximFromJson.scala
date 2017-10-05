package civilization.io.fromjson

import civilization.objects.P
import play.api.libs.json.{JsArray, JsValue, Json}

trait ImplicitMiximFromJson {

  implicit def toP(j: JsValue): P = convert[PJ](PJ(j))

}
