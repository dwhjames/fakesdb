
name := "fakesdb"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.0.v20120127",
  "junit" % "junit" % "4.8.2" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test",
  "com.amazonaws" % "aws-java-sdk" % "1.3.10" % "test"
)
