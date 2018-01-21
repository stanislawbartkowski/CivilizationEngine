package civilization

import io.readdir.TranslateMessage.translateMessage

package object message {

  object M extends Enumeration {
    type M = Value
    val POINTOUTSIDEBOARD, POINTONHIDDENTILE, POINTONWATER, CITYONWATER, POINTONBORDER,
    POINTISBORDERINGWITHHUTORVIALLAGE, CITYISBORDERINGWITHANOTHER, CAPITALALREADYBUILD, CITYLIMITEXCEEDED,
    NOUNUSEDHUTORVILLAGES, TILEALREADYREVEALED, NOTIMPLEMENTEDYET, CANNOTPUTFIGUREONWATER, STACKINGSIZEEXCEEDED,
    CANNOTSETFIGUREONCITY, NOTCITY, ONLYONESCIUTALLOWED, CANNOTGIVECOSTFORTHISOBJECT, CANNOTAFFORDOBJECT,
    LIMITFORARMIESEXCEEDED, LIMITFORSCOUTSEXCEEDED, ACTIONCANNOTBEEXECUTEDINTHISPHASE, DUPLICATECITYACTIONINTHISCITY,
    NOFIGURESBELONGINGTOCIVILIZATION, NUMBEROFSCOUTSLESS, NUMBEROFARMIESLESS, FIGURESMOVEDAGAIN, CANNOTFINDSTARTOFMOVE, TRAVELSPEEDEXCEEDED,
    CANNOTCROSSWATER, POINTNOTREVEALED, CANNOTSTOPINCITY, CANNOTSTOPINWATER, MOVENOTCONSECUTIVE, LASTMOVENOTENDED, CANNOTREVEALFROMTHISPOINT,
    TECHNOLOGYRESEARCHEDALREADY, CANNOTAFFORDTHISTECHNOLOGY, NOPLACEINTECHNOLOGYTREE, CANNOTRESEARCHMORETHENONCEINSINGLETURN, SHOULDBEDNFOFMOVENOW,
    MORECIVTTILESTHENCIVDECLARED, CANNOTFINDHOMETILEFORCIV, TOOMANYCIVREQUESTED, ITEMIZEACTIONNOTIMPLEMENTEDYET, THEREISNOCIVLIZATIONCITYATTHISPOINT,
    CITYSHOULDBEBUILDONSCOUT, HUTORVILLAGEATCITYOUTSKIRTS, FOREIGNFIGURESATCITYOUTSKIRTS, CANNOTSETFIGUREONALIENCIV, CANNOTSETFGUREONALIENCITY,
    NOTENOUGHTRADETOSPENDFORPROD, NOSCOUTATTHISPOINT,CANNOTSENTPRODTWICESCOUT, SCOUTISONCITYOUTSKIRT,CANNOTSETFIGUREONHUT,CANNOTSETFIGUREONVILLAGE,
    ALLUNITSUSED, COMMANDPARAMETERDOESNOTMATCH,THEREISNOHUTATRHISPOINT,SCOUTCANNOTEXPLOREHUT,CANNOTFINDFIGUREONATTACKINGSQUARE,
    CANNOTATTACKOWNFIGURE, CANNOTFINFANYTHINGONTHEQUARETOATTACK, CANNOTATTACKOWNCITY, CANNOTREMOVEUNITS, CANNOTUSERIRONNOW,
    IMPROPERUNITNUMBERTOPLAY,IMPROPERNUMBERTOPLAYUNIT,THEREISANOTHERUNITPLAYEDHERE, ENDOFBATTLEALREADY,BATTLEISNOTFINISHED,
    ENDOFBATTLEIMPLEMENTEDONLYFORVILLAGES,IMPROPERLOOTCANNOTGETTHISFROMLOSER, CAPITALCANBEBUILDONLYONHOMETILE,
    CANNOTBUYBUILDINGHERE= Value
  }

  case class Mess(val m: M.M, val o: AnyRef = null)

  case class FatalError(val m: Mess) extends Exception(translateMessage((m)))

}
