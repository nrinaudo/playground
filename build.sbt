
lazy val root = Project(id = "playground", base = file("."))
  .settings(moduleName := "root")
  .settings(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false
  )
  .aggregate(gadt, free, abstractions)

lazy val gadt = project
lazy val free = project.dependsOn(abstractions)
lazy val abstractions = project
