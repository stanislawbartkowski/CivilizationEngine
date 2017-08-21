package civilization.io.readdir

import civilization.action.Command
import civilization.message.{M, Mess}
import civilization.objects.TurnPhase
import play.api.libs.json.JsValue

object TranslateMessage {

  private def getCodePreface(messages : JsValue, co : Command, cityAction : Boolean) : String = {
    val codes :JsValue = readJSON("messages","actioncodes.json")
    val code = co.command.toString
    val v : Option[JsValue] = (codes \ code).toOption
    val desc : String = if (v.isEmpty) code else v.get.as[String]
    if (co.p != null) {
      val formatcode : String = if (cityAction) "CITYMANAGEMENTPREFACEPOINT" else "COMMANDPREFACEPOINT"
      val format: String = (messages \ formatcode).get.as[String]
      return format.format(desc, code, co.civ.toString, co.p.row, co.p.col)
    }
    val format: String = (messages \ "COMMANDPREFACEWITOUTPOINT").get.toString()
    format.format(desc, code, co.civ.toString)
  }

  def translateMessage(m : Mess): String = {
    val codes :JsValue = readJSON("messages","actioncodes.json")
    val messages : JsValue = readJSON("messages","messages.json")
    val v : Option[JsValue] = (messages \ m.m.toString).toOption
    if (v.isEmpty) return m.toString
    val format : String = v.get.as[String]
    m.m match {
      case M.ACTIONCANNOTBEEXECUTEDINTHISPHASE => {
        val co = m.o.asInstanceOf[(Command, TurnPhase.T, TurnPhase.T)]
        return getCodePreface(messages, co._1,true) + format.format(co._2, co._3)
      }
      case M.DUPLICATECITYACTIONINTHISCITY => {
        val co = m.o.asInstanceOf[(Command, Command)]
        return getCodePreface(messages, co._1,true) + format.format(getCodePreface(messages, co._2,true))
      }
    }
    return m.toString
  }


}
