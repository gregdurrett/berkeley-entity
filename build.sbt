import AssemblyKeys._ // put this at the top of the file

name := "berkeley-entity"

version := "1"

scalaVersion := "2.11.6"

assemblySettings

mainClass in assembly := Some("edu.berkeley.nlp.entity.Driver")

unmanagedResourceDirectories in Compile += { baseDirectory.value / "resources/" }

libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc"       % "2.2.5",
  "com.h2database"  %  "h2"                % "1.4.186",
  "ch.qos.logback"  %  "logback-classic"   % "1.1.2",
  "org.postgresql"  %  "postgresql"        % "9.4-1201-jdbc41"
)
