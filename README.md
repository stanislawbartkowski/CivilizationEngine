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

# The project

The solution is available as Intellij IDEA project.Steps to recreate:
- VCS -> Checkout from Version Control -> Git
- Git Repository URL : https://github.com/stanislawbartkowski/CivilizationEngine
- Parent Directory : according to your local environment
- Directory Name : CivilizationEngine

 ![screenshots/Zrzut ekranu z 2017-08-22 23-10-06.png|align=center]







