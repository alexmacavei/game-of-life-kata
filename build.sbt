scalaVersion := "2.12.8"

name := "game-of-life-kata"
organization := "ro.chronos"
version := "1.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")
