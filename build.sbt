name := "grok"

version := "0.1.2"

scalaVersion := "2.11.5"

libraryDependencies += "org.abego.treelayout" % "org.abego.treelayout.core" % "1.0.2"

antlr4Settings

antlr4GenListener in Antlr4 := false // default: true

antlr4GenVisitor in Antlr4 := true // default: false
