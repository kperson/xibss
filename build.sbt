lazy val xibss = (project in file(".")).
  settings(
    organization := "org.kelt",
    version := "1.0",
    scalaVersion := "2.11.7",
    maintainer := "Kelton Person <github.com/kperson>",
    parallelExecution in Test := false,
    packageDescription := "Converts xib files to gss and HTML"
  ).
  settings(libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )).enablePlugins(JavaAppPackaging)
