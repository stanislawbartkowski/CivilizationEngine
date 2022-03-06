TARGET=target/scala-2.13/CivilizationEngine-assembly-1.0.jar
mvn install:install-file -Dfile=$TARGET  -DgroupId=com.civlizationengine  -DartifactId=CivilizationEngine  -Dpackaging=jar -Dversion=1.0