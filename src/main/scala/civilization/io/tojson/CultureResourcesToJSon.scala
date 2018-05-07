package civilization.io.tojson

import civilization.gameboard._
import play.api.libs.json._

object CultureResourcesToJSon {

  def cultureToJSon(cul : CultureResources, details : Boolean) : JsValue =  Json.obj(
    "cardsno" -> cul.cards.length,
    "personsno" -> {cul.persons.length + cul.personsexposed.length},
    "personsused" ->cul.personsexposed,
    "cards" -> { if (details) cul.cards else JsNull},
    "persons" -> { if (details) cul.persons else JsNull}
  )

}
