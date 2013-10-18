scalaVersion := "2.10.3"

name := "Forge Provider"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "optional")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.RC1" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"