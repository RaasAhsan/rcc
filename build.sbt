
lazy val root = project
  .in(file("."))
  .aggregate(compiler)

lazy val compiler = project
  .in(file("modules/compiler"))
  .settings(
    fork in run := true,
    libraryDependencies ++= Seq(
      Dependencies.catsParse
    )
  )
