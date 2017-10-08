package civilization.io.tojson

import civilization.objects._
import play.api.libs.json.{JsValue, Json}

trait ImplicitMiximToJson {

  implicit def writesPoint(p: P): JsValue = Json.toJson(p)

  implicit def toSeqP(li : Seq[P]) : Seq[JsValue] =
    li.map(p => writesPoint(p))


  implicit def CPtoJS(c : CommandParams) : JsValue = {
    if (c.p.isDefined)
      if (c.param.isDefined) Json.obj(
        S.p -> c.p,
        S.param -> c.param
      )
      else Json.obj( S.p -> c.p)
      // either p or param is defined, cannot be both None
    else Json.obj(S.param -> c.param)
  }

  implicit def toJSArrayParams(li : Seq[CommandParams]) : Seq[JsValue] = li.map(CPtoJS)


}