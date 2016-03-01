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
  )).enablePlugins(JavaAppPackaging)
