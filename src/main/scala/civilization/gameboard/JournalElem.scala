package civilization.gameboard

import civilization.message.J.J
import civilization.objects._

object JournalElem {

  object JournalPrivacy extends Enumeration {
    type T = Value
    val Private, NotPrivate, Public = Value
  }

  case class JournalArtifacts(val tech: Option[TechnologyName.T], val card: Option[CultureCardName.T], val res: Option[Resource.T], val building: Option[BuildingName.T], val wonder: Option[Wonders.T])

  case class JournalElem(val l: J, val pha: TurnPhase.T, val roundno: Int, val civ: Civilization.T, val params: Seq[String], val artif: JournalArtifacts, val priv: JournalPrivacy.T = JournalPrivacy.Public, val jparams: Option[CommandParams] = None)

  def constructJA(p: Any): JournalArtifacts = {
    p match {
      case tech: TechnologyName.T => JournalArtifacts(Some(tech), None, None, None, None)
      case build: BuildingName.T => JournalArtifacts(None, None, None, Some(build), None)
      case card: CultureCardName.T => JournalArtifacts(None, Some(card), None, None, None)
      case res: Resource.T => JournalArtifacts(None, None, Some(res), None, None)
      case w: Wonders.T => JournalArtifacts(None, None, None, None, Some(w))
      case _ => constructJA
    }
  }

  def constructJA: JournalArtifacts = JournalArtifacts(None, None, None, None, None)

}
