package civilization.gameboard

object CultureTrack {

  case class CultureTrackCost(val culture : Int, val trade : Int)

  case class CultureTrackSegment(val last : Int, val cost : CultureTrackCost, val greatperson : Seq[Int])

  type CultureTrack = Array[CultureTrackSegment]

}
