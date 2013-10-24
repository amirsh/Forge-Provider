scalaVersion := "2.10.3"

name := "forge-provider"

version := "0.1"

organization := "ch.epfl.lamp"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "optional")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.RC1" % "test"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

libraryDependencies += "ch.epfl.lamp" %% "lms-lifter" % "0.1"