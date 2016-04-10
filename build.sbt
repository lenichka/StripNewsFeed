name := "stripNewsFeed"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  // Common Dependencies
  "org.json4s" %% "json4s-jackson" % "3.3.0",

  // Test Dependencies
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "commons-io" % "commons-io" % "2.4" % "test"
)