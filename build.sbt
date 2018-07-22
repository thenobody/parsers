name := "parser"

version := "0.1"

scalaVersion := "2.12.6"

val catsVersion = "1.1.0"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion
val catsFree = "org.typelevel" %% "cats-free" % catsVersion

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  catsCore,
  catsFree
)