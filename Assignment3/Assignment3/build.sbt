name := "Assignment3Solution"

version := "0.1"

scalaVersion := "2.12.7"

lazy val sparkDependencies = Seq(
  //spark context, spark config ...
  "org.apache.spark" %% "spark-core" % "2.4.0",
  //dataframe ...
  "org.apache.spark" %% "spark-sql" % "2.4.0"
)

//testing dependencies
lazy val testDependencies = Seq(
  "junit" % "junit" % "4.12" % "test",
  "org.scalactic" %% "scalactic" % "3.0.5" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

libraryDependencies ++= testDependencies
libraryDependencies ++= sparkDependencies
//
//// json deserialization
//// https://mvnrepository.com/artifact/com.fasterxml.jackson.module/jackson-module-scala
//libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.6"
//// https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-core
//libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.9.6"