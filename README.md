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

The engine does not contain any user interface, the user interface is developed as a separate project. It keeps all game logic and state and executes commands to move from one game stage to another and provides current state to the clients to keep user interface synchronized.
Only single player test game is available.
The output artifact is CivilizationEngine.jar
The only dependency is play_json (https://www.playframework.com/documentation/2.6.x/ScalaJson). Check build.sbt file.

# The project

The solution is available as Intellij IDEA project.Steps to recreate:
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
* customize properties: vi civilizationengine.properties
* ant -f  civilizationengine.xml
* pick up the result: ls out/artifacts/CivilizationEngine/

# Brief interface result

* package civilization.I. All parameters and result are passed as JSON objects

| Method | Parameters | Result | Example | Description |
| -------|:----------:| :-----:|:-------:|:------------|
| getData(LISTOFCIV | null | List of civilizations available | val l : String = getData(LISTOFCIV,null) | Should be called at then beginning of game to allow players to choose civilization they want to run
| getData(REGISTERGAME | civilization, string | Token, string | val token:String = getData(REGISTERGAME,"Germany") | The command initialize map, register civilization as game owner and return unique token. This token should be used in the next calls
| getData(GETBOARDGAME | token, string | Current state of game as JSON object | val board:String = getData(GETBOARDGAME,"xxx") | Returns the gameboard reflecting the current state. 
| executeCommand | token: String, action:String command to execute, row,col : Int: position on map, jsparam : additional parameters for command to execute. Content is specific for a particular command | null if success otherwise the error description | Executes command and moves game from one state to another. After success call getData(GETBOARDGAME) to receive current game state.
| itemizeCommand | token :String, command String | Returns list of possible parameters for the command. The content depends on a particular command | val l : String = itemizeCommand("xxxx","SETCAPITAL") | Returns more detailed information. For instance: if the command is SETCAPITAL the command will return a list of all points where the capital can be build. Can be used by user interface to customize screen.




 





