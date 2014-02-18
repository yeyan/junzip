name := "junzip"

version := "1.0"

scalaVersion := "2.10.3"

fork := true

scalariformSettings

libraryDependencies += "commons-io" % "commons-io" % "2.0"
            
libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

resolvers += Resolver.sonatypeRepo("public")

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "com.googlecode.lanterna" % "lanterna" % "2.1.7"

