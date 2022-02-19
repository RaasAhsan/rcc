
lazy val root = project
  .in(file("."))
  .aggregate(cli, compiler)

lazy val cli = project
  .in(file("modules/cli"))
  .settings(
    fork / run := true,
    libraryDependencies ++= Seq(
      Dependencies.decline
    )
  )
  .dependsOn(compiler)

lazy val compiler = project
  .in(file("modules/compiler"))
  .settings(
    fork / run := true,
    libraryDependencies ++= Seq(
      Dependencies.catsParse,
      Dependencies.osLib
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-unchecked",
      "-deprecation",
      "-Xfatal-warnings"
    )
  )
  