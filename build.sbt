val scala3Version = "3.7.2"

val executable = sys.props.getOrElse("EXECUTABLE", "xcount")
val hw         = sys.props.getOrElse("HW", "hw1")

lazy val root = project
  .in(file("."))
  .settings(
    name                       := hw,
    version                    := "0.1.0-SNAPSHOT",
    scalaVersion               := scala3Version,
    assembly / assemblyJarName := executable,

    // sbt-coverage
    coverageEnabled := true,
    coverageExcludedPackages := "sexprs",
    
    // wartremover
    // wartremoverWarnings ++= Warts.allBut(Wart.Throw),
    Compile / compile / wartremoverWarnings ++= Warts.allBut(Wart.Throw),
    wartremoverExcluded += baseDirectory.value / "src" / "main" / "scala" / "sexprs",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
