# CivilizationEngine

https://www.fantasyflightgames.com/en/products/civilization/

It is a Scala project to implement Civilization The Board Game engine. Only base game, without expansions "About Wisdom and Warfare" and "Fame and Fortune"
It is the beginning, for the time being only the following actions are implemented:
* map: squares and tiles
* capital setting
* city setting
* buying figures: armies and scouts
* figures movement
* tile revealing
* explore hut
* harvest resource
* explore hut
* fight village
* fight between players

The engine does not contain any user interface, the user interface is developed as a separate project. [https://github.com/stanislawbartkowski/CivilizationUI] It keeps all game logic and state and executes commands to move from one game stage to another and provides the current state to the clients to keep user interface synchronized.
Only single player test game is available.
The output artifact is CivilizationEngine.jar
The only dependency is play_json (https://www.playframework.com/documentation/2.6.x/ScalaJson). Check build.sbt file.

# The project

The solution is available as Intellij IDEA project. Steps to recreate:
* VCS -> Checkout from Version Control -> Git
- Git Repository URL : https://github.com/stanislawbartkowski/CivilizationEngine
- Parent Directory : according to your local environment
- Directory Name : CivilizationEngine

 ![](https://github.com/stanislawbartkowski/CivilizationEngine/blob/master/screenshots/Zrzut%20ekranu%20z%202017-08-22%2023-10-06.png)
 * Clone
 
 ![](https://github.com/stanislawbartkowski/CivilizationEngine/blob/master/screenshots/Zrzut%20ekranu%20z%202017-08-22%2023-30-13.png)
 
# Create artifact from command line.

* git clone https://github.com/stanislawbartkowski/CivilizationEngine.git
* cd CivilizationEngine
* customize properties: vi build.properties
* ant 
* pick up the result: ls out/artifacts/CivilizationEngine/
IMPORTANT: seems not working properly, IntelliJ ant build does not generate scalac tasks

# Brief interface description

* package civilization.II. All parameters and result are passed as JSON objects


## getData(LISTOFCIV,null)

* Result : List of civilizations available
* Result format: JSON Array, ['civ1','civ2','civ3'...]
* Usage example : val l : String = getData(LISTOFCIV,null)
* Description: Should be called at then beginning of game to allow players to choose civilization they want to run.

## getData(REGISTERGAME,civilization)
* Result: Token
* Result format: string
* Usage example: val token:String = getData(REGISTERGAME,"Germany")
* Description: The command initialize map, register civilization as game owner and return the unique token. The token should be used in the next calls

## getData(GETBOARDGAME,token)
* Parameter: token returned by REGISTER game
* Result: Current state of game as JSON object
* Usage example: val board:String = getData(GETBOARDGAME,"secret token")
* Description : Returns the gameboard reflecting the current state. 

# Format, JSON
```JSON
{ {
  "board" : {
    "map" : [ [
       .....
    ]]
   },
   
   "game" : {
      "roundno" : 0,
      "phase" : "Trade",
      "active" : "China"
    },
    
   "units" : {
      "units" : [ {
        "name" : "Artillery",
        "num" : 23
      }, {
        "name" : "Infantry",
        "num" : 23
      }, {
        "name" : "Mounted",
        "num" : 23
      }, {
        "name" : "Aircraft",
        "num" : 26
      } ]
    },
    
    "killedunits" : {
      "units" : [ {
        "name" : "Artillery",
        "num" : 0
      }, {
        "name" : "Infantry",
        "num" : 0
      }, {
        "name" : "Mounted",
        "num" : 0
      }, {
        "name" : "Aircraft",
        "num" : 0
      } ],
      "list" : [ ]
    },
    
    [
       {"resource":"Incense","num":25},
       {"resource":"Spy","num":0},
       {"resource":"Coin","num":25},
       {"resource":"Silk","num":25}, 
       {"resource":"Iron","num":1},
       {"resource":"Uranium","num":0},
       {"resource":"Wheat","num":25}
     ]

      "you" : {
      "civ" : "China",
      "trade" : 6,
      "coins" : 4,
      "combatbonus" : 3,
      "commands" : [ {
        "command" : "ENDOFPHASE"
      } ],
      
      "citylimit" : 1,
      "armieslimit" : 6,
      "scoutslimit" : 2,
      "tradeforprod" : 3,
      
      "units" : {
        "units" : [ {
          "name" : "Artillery",
          "num" : 2,
          "militarystrength" : 0
        }, {
          "name" : "Infantry",
          "num" : 0,
          "militarystrength" : 0
        }, {
          "name" : "Mounted",
          "num" : 0,
          "militarystrength" : 0
        }, {
          "name" : "Aircraft",
          "num" : 0,
          "militarystrength" : 0
        } ],      
        
        "list" : [ {
          "name" : "Infantry",
          "strength" : [ 1, 2, 3, 4 ]
        }, {
          "name" : "Artillery",
          "strength" : [ 1, 3, 4, 6 ]
        }, {
          "name" : "Mounted",
          "strength" : [ 2, 3, 4, 5 ]
        } ],
        
      "resources" : [ {
        "resource" : "Incense",
        "num" : 0
      }, {
        "resource" : "Spy",
        "num" : 0
      }, {
        "resource" : "Coin",
        "num" : 0
      }, {
        "resource" : "Silk",
        "num" : 0
      }, {
        "resource" : "Iron",
        "num" : 1
      }, {
        "resource" : "Uranium",
        "num" : 0
      }, {
        "resource" : "Wheat",
        "num" : 0
      } 
      ]
      },
    "tech" : [ {} ] 
    "hutvillages" : {
        "Hut" : 1,
        "list" : [ {
          "hv" : "Hut",
          "resource" : "Spy"
        } ]
      }
    },
  }
```
# Technology
```JSON
{
      "name" : "Construction",
      "gover" : null,
      "level" : 2,
      "ni" : true
}      
```
* name : Name of the technology
* gover : If not null then technology enables government change
* level : Technology level (1 - 4)
* ni : true, not implemented yet

# Government and technology list for player
```JSON
    "you" : {
      "tech" : [ {
        "tech" : {
          "name" : "CodeOfLaw",
          "gover" : "Republic",
          "level" : 1,
          "ni" : true
        },
        "initial" : true,
        "level" : 1
      } ],
      "gover" : "Republic",
      "civ" : "Rome",
      "trade" : 0,
```
* tech : Technology
* tech\tech : Technology definiton
* initial : if start technology
* level : technology level, if initial always 1

gover: Government

# All technologies
```JSON
    "tech" : [ {
      "name" : "Irrigation",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Construction",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "MetalCasting",
      "gover" : null,
      "level" : 3,
      "ni" : true
    }, {
      "name" : "Communism",
      "gover" : "Communism",
      "level" : 3,
      "ni" : true
    }, {
      "name" : "MilitaryScience",
      "gover" : null,
      "level" : 3,
      "ni" : true
    }, {
      "name" : "SteamPower",
      "gover" : null,
      "level" : 3,
      "ni" : true
    }, {
      "name" : "GunPowder",
      "gover" : null,
      "level" : 3,
      "ni" : true
    }, {
      "name" : "NuclearFusion",
      "gover" : null,
      "level" : 4,
      "ni" : true
    }, {
      "name" : "ReplaceableParts",
      "gover" : null,
      "level" : 4,
      "ni" : true
    }, {
      "name" : "Ballistics",
      "gover" : null,
      "level" : 4,
      "ni" : true
    }, {
      "name" : "MessMedia",
      "gover" : null,
      "level" : 4,
      "ni" : true
    }, {
      "name" : "Computers",
      "gover" : null,
      "level" : 4,
      "ni" : true
    }, {
      "name" : "Flight",
      "gover" : null,
      "level" : 4,
      "ni" : true
    }, {
      "name" : "PrintingPress",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Democracy",
      "gover" : "Democracy",
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Monarchy",
      "gover" : "Monarchy",
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Sailing",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Mathematics",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "CivilService",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Engineering",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Metallurgy",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "CodeOfLaw",
      "gover" : "Republic",
      "level" : 1,
      "ni" : true
    }, {
      "name" : "HorsebackRiding",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Writing",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Pottery",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Philosophy",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Navigation",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "AnimalHusbandry",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Currency",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Masonry",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Plastics",
      "gover" : null,
      "level" : 4,
      "ni" : true
    }, {
      "name" : "Ecology",
      "gover" : null,
      "level" : 3,
      "ni" : true
    }, {
      "name" : "Mysticism",
      "gover" : null,
      "level" : 2,
      "ni" : true
    }, {
      "name" : "RailRoad",
      "gover" : null,
      "level" : 3,
      "ni" : true
    }, {
      "name" : "Biology",
      "gover" : null,
      "level" : 3,
      "ni" : true
    }, {
      "name" : "Theology",
      "gover" : "Fundamentalism",
      "level" : 3,
      "ni" : true
    }, {
      "name" : "Agriculture",
      "gover" : null,
      "level" : 1,
      "ni" : true
    }, {
      "name" : "Chivalry",
      "gover" : "Feudalism",
      "level" : 2,
      "ni" : true
    }, {
      "name" : "Banking",
      "gover" : null,
      "level" : 3,
      "ni" : true
    } ],
```

# Battle
## Start
```JSON
    "battle" : {
      "endofbattle" : false,
      "attackerwinner" : false,
      "attacker" : {
        "combatbonus" : 0,
        "front" : [ null, null, null, null, null, null ],
        "canuseiron" : false,
        "ironused" : -1,
        "killedunits" : [ ],
        "waiting" : {
          "units" : [ {
            "name" : "Artillery",
            "num" : 1,
            "militarystrength" : 0
          }, {
            "name" : "Infantry",
            "num" : 1,
            "militarystrength" : 0
          }, {
            "name" : "Mounted",
            "num" : 1,
            "militarystrength" : 0
          }, {
            "name" : "Aircraft",
            "num" : 0,
            "militarystrength" : 0
          } ],
          "list" : [ {
            "name" : "Artillery",
            "strength" : [ 2, 3, 4, 6 ]
          }, {
            "name" : "Mounted",
            "strength" : [ 1, 2, 4, 5 ]
          }, {
            "name" : "Infantry",
            "strength" : [ 1, 3, 4, 6 ]
          } ]
        },
        "you" : true,
        "turn" : false,
        "points" : 4
      },
      "defender" : {
        "combatbonus" : 0,
        "front" : [ null, null, null, null, null, null ],
        "canuseiron" : false,
        "ironused" : -1,
        "killedunits" : [ ],
        "waiting" : {
          "units" : [ {
            "name" : "Artillery",
            "num" : 1,
            "militarystrength" : 0
          }, {
            "name" : "Infantry",
            "num" : 1,
            "militarystrength" : 0
          }, {
            "name" : "Mounted",
            "num" : 1,
            "militarystrength" : 0
          }, {
            "name" : "Aircraft",
            "num" : 0,
            "militarystrength" : 0
          } ],
          "list" : [ {
            "name" : "Infantry",
            "strength" : [ 1, 3, 4, 6 ]
          }, {
            "name" : "Artillery",
            "strength" : [ 2, 3, 4, 6 ]
          }, {
            "name" : "Mounted",
            "strength" : [ 1, 2, 4, 5 ]
          } ]
        },
        "you" : true,
        "turn" : true,
        "points" : 4
      }
    }
  }
}
```
* endofbattle 
  * false: in the miffle of the battle
  * true: end of the battle, attackerwinner: true or false. After that executeCommand(ENDBATTLE) should be submitted.
  
# Buldings
It is the list of building in the market. The number of buildings available is assigned to undeveloped building.

```json
 "buildings" : [ {
      "name" : "Temple",
      "num" : 5
    }, {
      "name" : "TradingPost",
      "num" : 6
    }, {
      "name" : "Library",
      "num" : 6
    }, {
      "name" : "Barracks",
      "num" : 5
    }, {
      "name" : "Granary",
      "num" : 6
    }, {
      "name" : "Shipyard",
      "num" : 5
    }, {
      "name" : "Market",
      "num" : 5
    }, {
      "name" : "Workshop",
      "num" : 6
    }, {
      "name" : "Harbor",
      "num" : 10
    } ]
```

# Wonders
List of buildings and wonders to be built.
```JSON
[ {
  "p" : {
    "row" : 2,
    "col" : 2
  },
  "list" : [ {
    "p" : {
      "row" : 1,
      "col" : 1
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 1,
      "col" : 2
    },
    "wonder" : "Stonehenge",
    "list" : [ {
      "row" : 1,
      "col" : 2
    } ]
  }, {
    "p" : {
      "row" : 1,
      "col" : 3
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 3,
      "col" : 1
    },
    "wonder" : "Stonehenge",
    "list" : [ {
      "row" : 3,
      "col" : 1
    } ]
  }, {
    "p" : {
      "row" : 2,
      "col" : 1
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  } ]
}, {
  "p" : {
    "row" : 5,
    "col" : 3
  },
  "list" : [ {
    "p" : {
      "row" : 4,
      "col" : 2
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 4,
      "col" : 3
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 4,
      "col" : 4
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 6,
      "col" : 2
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 6,
      "col" : 4
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 5,
      "col" : 2
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  }, {
    "p" : {
      "row" : 5,
      "col" : 4
    },
    "wonder" : "Stonehenge",
    "list" : [ ]
  } ]
} ]
```

# Units
Summary, number of units. Opposite players exposes only this information to you
Example
```JSON
     {
      "name" : "Artillery",
       "num" : 1,
       "militarystrength" : 0
      }
```
Single artillery unit. militarystrenght : current tech advancement for this type of unit, number 0-3

You receive also detailed list of units.
```JSON
       {
          "name" : "Infantry",
          "strength" : [ 1, 2, 3, 4 ]
        },
```
Infantry unit with power specified.

# Hut and Villages explored
```JSON
  "hutvillages" : {
        "Hut" : 1,
        "list" : [ {
          "hv" : "Hut",
          "resource" : "Spy"
        } ]
      }
```
Number of units (Hut or Villages) and detailed list of discovered resources.
In this example:
One Hut and Spy discovered

## executeCommand(token,actionname,row,col,jsparam)
* Parameters
  * token : Returned by REGISTERGAME
  * action : Command to execute: SETCAPITAL, SETSITY, SPENDPRODUCTION
  * row,col : Square position identyfying the object. Depends on the action. For instance: SETCITY - square where city is supposed to be built
  * jsparam : Additional parameter in JSON format. Depends on the action. For instance: STARTMODE - fugures or armies to move moved. Null if parameter is not required.
* Result: if null then command executed succesfully. If not null, the command failed and the result contains failure description
* Usage example: executeCommand("secret token","SETCITY",2,2,null)
* Description: Executes command and moves game from one state to another. After success call getData(GETBOARDGAME) to receive current game state. 

## itemizeCommand(token,command)
* Parameters: token and command name
* Returns: List of possible parameters for the command. The content depends on a particular command
* Usage example: val l : String = itemizeCommand("secret token","SETCAPITAL")
* Description: Returns more detailed information. For instance: if the command is SETCAPITAL the command will return a list of all points where the capital can be built. Can be used by user interface to customize screen.

# executeCommand format

## SENDPRODUCTION,HARVESTRESOURCE
* executeCommand(token,"SENDPRODUCTION",row,col,jsparam: { "row" : int, "col" : int })
* executeCommand(token,"HARVESTRESOURCE",row,col,jsparam: { "row" : int, "col" : int })
* Parameters
  * row,col : city position where production is to be sent
  * jsparam : scout square coordinates to be harvested
* Usage example:
  * executeCommand("secret token","SENDPRODUCTION",2,2,"{"row" : 5, "col" : 3})
  * Send production from square (5,3) to city/capital (2,2)
  
## UNDOSENDPRODUCTION
* executeCommand(token,"UNDOSENDPRODUCTION",row,col,jsparam : jsparam: { "row" : int, "col" : int })
* Parameters:
  * row,col : city to undo last send production command
  * jsparam : scout square to be undone
* Usage example:  
  * executeCommand("secret token","UNSENDPRODUCTION",2,2,"{"row" : 5, "col" : 3})
  * Undo send production in city (2,2) from square (5,3)
  
## ATTACK
* executeCommand(token,"ATTACK",row, col)
* Parameters:
 * row,col : square to attack. Can be village or alient unit or city. Currently only attacking village is implemented. The attacking unit is the current unix in move phase
* Usage example:
  * executeCommand("secret token","ATTACK",4,0,null)
  * Start the battle involving village at square (4,0)
  
## PLAYUNIT
## PLAYUNITIRON
* executeCommand(token,"PLAYUNIT",indexfrom, indexto,null)
* Parameters:
  * indexfrom : position of the unit on "waiting" line 
  * indexto: position on "fighting" line where unit is played to
* Usage example:
  * executeCommand("secret token",0,0,null)
  * waiting unit from position 0 is moving to position 0 in fighting line.

## ENDBATTLE
* executeCommand("secret token",-1,-1,null)
* Parameters: no
  * in the future JSON par value should contain the winner reward
* Usage example:
  * executeCommand("ENDBATTLE",-1,-1,null)
  
## BUYBUILDING
* executeCommand("secret token","BUYBUILIGING",row,col, { "row" : row, "col" : col, "building" : "building name"})
* Parameters:
 *  row,col : city
 *  { "row","col", "building"} : square at the outskirt of the city where bulding is build and set up
* Usage example:
 * executeCommand("BUYBUILDING",0,4,{{"p":{"row":0,"col":4},"building":"Temple"})
 
Remark: The same command is used to build a new building on an empty square and as a replacement of existing building. For replacement, the previous building is removed and new is nailed down. Also for a limited building. The previous limited building is removed regardless where it is standind and a new building is put.  

## POTTERYACTION,PHILOSOPHYACTION

Spend resources or hut/villages and put a coin on the technology

* executeCommand("secret token","POTTERYACTION",row,col,[{"hv" : "Village/Hut/null","resource" : "resource name"}])
* executeCommand("secret token","PHILOSOPHYACTION",row,col,[{"hv" : "Village/Hut/null","resource" : "resource name"}])
* Parameters:
* row,col : city
* [ { .... } ] : list of resources to spend. Format of a single resource:
{ "hv" : "Hut/Village", "resource" : "resource name"} : Hut or village with resource
{ "resource" : "resource name" } : resource itself, outside Hut or Village

* Usage example:
* executeCommand("secret token","POTTERYACTION",1,2,[{"hv" : "Hut","resource" : "Spy"}, {"resource" : "Iron"])

 Spend Hut with Spy and Iron to put coin on Pottery technology. The action is executed in city (1,2)

## DEVOUTTOCULTURE

Devout city to culture. City can be helped by scout standing on the squre having resource token.

* executeCommand("secret token","DEVOUTTOCULTURE",row,col,[{"row":row,"col":col])

*Parameters:
* row,col : city to be devouted to culture
* list : list of supporting scouts. If no scout is supporting empty list should be passed

# itemizeCommand format

For every command the engine can return itemization, list of possible moves. The format is different for every command.

## SENDPRODUCTION, UNDOSENDPRODUCTION, HARVESTRESOURCES

List of pairs, city and scout or square. City, where production can be sent and scout, a square to be harvested or undone

Format:
\[ { "p" : {"row" : int, "col" : int}, "param" : { "row" : int, "col" : int } } \]

Sample

\[ { "p" : { "row" : 2, "col" : 2 }, "param" : { "row" : 1, "col" : 5 } } \]

Production from square (1,5) can be sent to city (2.2)

## UNDOSPENDTRADE, UNDOSPENDTRADE

List of cities where unit can be bought. 

Format:
\[ {"p": {"row" : int,"col" : int}} \]

Sample:

\[ {"p: : {"row" : 2,"col" : 2}},{ "p" : {"row" : 5,"col" : 3}} \]

A unit can be bought in two cities : (2,2) and (5,3)

## EXPLOREHUT

List of huts (always single) ready to be exlored

Format:

 {"p" : { "row" : int, "col" : int}, "explore" : \[{ "row" : int, "col" : int }\] }

Sample

 {"p" : { "row" : 2, "col" : 5}, "explore" : \[{ "row" : 2, "col" : 4 }\] }
 
 Scout or figure at point (2,5) can explore hit (2,4)
 
 ## BUYBUILDING
 
Returns a list of cities and list of buildings possible to buy and build on the outskirts of a particular city.

Example below:

```JSON
 [{"p":{"row":1,"col":5},
```
City at square (1,5)
Around a city Market and Temple can be built.
```JSON
       {"p":{"row":0,"col":4},"building":"Market","list":[]},
```
Market can be build at square (0,4).

```
"list":[]
```
For every building, there is a list of buildings to be pulled down if new building is puit there. The list could contain 1 (for replacement) or 2 elements. The second case is for "limited" building marked with star sign. In order to build a next limited building, the previous one should be removed.
       
 ```JSON
 [{"p":{"row":1,"col":5},
   "list":[
       {"p":{"row":0,"col":4},"building":"Market","list":[]},
       {"p":{"row":0,"col":4},"building":"Temple","list":[]},
       {"p":{"row":2,"col":4},"building":"Market","list":[]},
       {"p":{"row":2,"col":4},"building":"Temple","list":[]},
       {"p":{"row":2,"col":6},"building":"Market","list":[]},
       {"p":{"row":2,"col":6},"building":"Temple","list":[]},
       {"p":{"row":1,"col":4},"building":"Market","list":[]},
       {"p":{"row":1,"col":4},"building":"Temple","list":[]},
       {"p":{"row":1,"col":6},"building":"Market","list":[]},
       {"p":{"row":1,"col":6},"building":"Temple","list":[]}
     ]
  }
  ]
```
## POTTERYACTION,PHILOSOPHYACTION

Returns list of cities where action is possible to execute

Example:

```JSON
 [{"p":{"row":1,"col":5},
```
Action can be execute in city (1,5)

## DEVOUTTOCULTURE

Return list of cities accompanied by list of scout ready to send the culture to the city

Example

```JSON
[{"p":{"row":1,"col":1},"list":[]}]
```
City (1,1) can devout itself to culture. No scout can support.
