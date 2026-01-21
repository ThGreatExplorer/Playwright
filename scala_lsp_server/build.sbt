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

    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "versions", _, "module-info.class") =>
        MergeStrategy.discard

      case "reference.conf" =>
        MergeStrategy.concat

      case PathList("META-INF", xs @ _*) =>
        xs.map(_.toLowerCase) match {
          case ("manifest.mf" :: Nil)               => MergeStrategy.discard
          case name :: Nil if name.endsWith(".sf")  => MergeStrategy.discard
          case name :: Nil if name.endsWith(".dsa") => MergeStrategy.discard
          case name :: Nil if name.endsWith(".rsa") => MergeStrategy.discard
          case _                                    => MergeStrategy.first
        }

      case x =>
        MergeStrategy.first
    },

    // sbt-coverage
    coverageExcludedPackages := "sexprs",
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 90,
    coverageMinimumBranchTotal := 90,
    
    // wartremover
    // wartremoverWarnings ++= Warts.allBut(Wart.Throw),
    // Compile / compile / wartremoverWarnings ++= Warts.allBut(Wart.Throw, Wart.Recursion, 
    // Wart.StringPlusAny, Wart.Any, Wart.Equals, Wart.IterableOps, Wart.MutableDataStructures,
    // Wart.Nothing),
    // wartremoverExcluded += baseDirectory.value / "src" / "main" / "scala" / "sexprs",
    // wartremoverExcluded += baseDirectory.value / "src" / "main" / "scala" / "server",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.24.0",
  )
