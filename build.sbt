import AssemblyKeys._ // put this at the top of the file

name := "berkeley-entity"

version := "1"

scalaVersion := "2.11.6"

assemblySettings

mainClass in assembly := Some("edu.berkeley.nlp.entity.Driver")

unmanagedResourceDirectories in Compile += { baseDirectory.value / "resources/" }
