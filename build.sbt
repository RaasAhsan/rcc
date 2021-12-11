
lazy val root = project.in(file("."))
  .aggregate(compiler)

lazy val compiler = project
  .in(file("modules/compiler"))
  .settings(
    libraryDependencies ++= SeQ(
      Dependencies.catsParse
    )
  )
