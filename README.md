# CivilizationEngine

https://www.fantasyflightgames.com/en/products/civilization/

Detailed command description: WIKI https://github.com/stanislawbartkowski/CivilizationEngine/wiki

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

# Redis

> podman pull docker.io/library/redis <br>
> podman run -d --name redis -p 6379:6379 redis <br>

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

# Create jar from command line

Install SBT: https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html

> sbt clean assembly<br>

Result

> ll target/scala-2.13/CivilizationEngine-assembly-1.0.jar<br>

 
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
  
## UNDOSENDPRODUCTION
* executeCommand(token,"UNDOSENDPRODUCTION",row,col,jsparam : jsparam: { "row" : int, "col" : int })
* Parameters:
  * row,col : city to undo last send production command
  * jsparam : scout square to be undone
* Usage example:  
  * executeCommand("secret token","UNSENDPRODUCTION",2,2,"{"row" : 5, "col" : 3})
  * Undo send production in city (2,2) from square (5,3)
    
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

# itemizeCommand format

For every command the engine can return itemization, list of possible moves. The format is different for every command.


## POTTERYACTION,PHILOSOPHYACTION

Returns list of cities where action is possible to execute

Example:

```JSON
 [{"p":{"row":1,"col":5},
```
Action can be execute in city (1,5)
