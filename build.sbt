name := "parser"

version := "0.1"

scalaVersion := "2.12.6"

val catsVersion = "1.1.0"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
  "org.typelevel" %% "cats-effect" % "1.0.0-RC2",
)