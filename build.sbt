name := "palindromes"

version := "1.0.0-SNAPHOT"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

resolvers ++= Seq(
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.2.3" % "test"
)

seq(ScctPlugin.instrumentSettings : _*)

