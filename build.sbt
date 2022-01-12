
lazy val root = project
  .in(file("."))
  .aggregate(cli, compiler, llvm)

lazy val cli = project
  .in(file("modules/cli"))
  .settings(
    fork / run := true,
    libraryDependencies ++= Seq(
      Dependencies.decline
    )
  )
  .dependsOn(compiler)

lazy val llvm = project
  .in(file("modules/llvm"))

lazy val compiler = project
  .in(file("modules/compiler"))
  .settings(
    fork / run := true,
    libraryDependencies ++= Seq(
      Dependencies.catsParse,
      Dependencies.osLib
    )
  )
  .dependsOn(llvm)
