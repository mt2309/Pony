name := "Pony"

version := "1.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" , "-Xlint", "-Ywarn-all", "-Xverify", "-Ywarn-dead-code" , "-Ywarn-all", "-encoding", "utf8")

scalacOptions in (Compile, doc) ++= Seq("-diagrams")

seq(Twirl.settings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0"

libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.5.0"

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"
