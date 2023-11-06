import com.typesafe.tools.mima.core._

lazy val scalaTestVersion = "3.2.16"

lazy val scalaVersion213 = "2.13.10"
lazy val scalaVersion3 = "3.3.0"
lazy val scalaVersionInBuild = scalaVersion213

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / libraryDependencySchemes ++= Seq(
  "io.circe" % "circe-core" % "always",
  "io.circe" % "circe-numbers" % "always",
  "org.typelevel" % "cats-core" % "early-semver",
  "org.typelevel" % "cats-kernel" % "early-semver",
  "com.lihaoyi" % "ujson" % "early-semver",
  "com.lihaoyi" % "upickle-core" % "early-semver",
  "com.github.plokhotnyuk.jsoniter-scala" % "jsoniter-scala-core" % "early-semver"
)

ThisBuild / versionPolicyIntention := Compatibility.None
ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some(
  raw"^\d+\.\d+\.\d+\+\d+".r
) // Support for versions generated by sbt-dynver

lazy val commonSettings = Seq(
  crossScalaVersions := Seq(scalaVersion213, scalaVersion3),
  scalaVersion := scalaVersionInBuild,
  Test / parallelExecution := false,
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) =>
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:existentials",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings" // Fail the compilation if there are any warnings.
      )
    case Some((2, _)) =>
      Seq(
        "-opt:l:method",
        "-opt:l:inline",
        "-opt-inline-from:org.saddle.**",
        "-opt-warnings",
        "-Wopt",
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-encoding",
        "utf-8", // Specify character encoding used by source files.
        "-feature", // Emit warning and location for usages of features that should be imported explicitly.
        "-language:postfixOps",
        "-language:existentials",
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
        "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
        "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
        "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
        "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
        "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
        "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
        "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
        "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
        "-Xlint:option-implicit", // Option.apply used implicit view.
        "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
        "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
        "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
        "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
        "-Ywarn-dead-code", // Warn when dead code is identified.
        // "-Ywarn-numeric-widen", // Warn when numerics are widened.
        "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
        "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals", // Warn if a local definition is unused.
        "-Ywarn-unused:params", // Warn if a value parameter is unused.
        "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates" // Warn if a private member is unused.
      )
    case _ => ???
  }),
  Compile / console / scalacOptions ~= (_ filterNot (_ == "-Xfatal-warnings"))
) ++ Seq(
  organization := "io.github.pityka",
  licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
  Global / pomExtra := {
    <url>https://github.com/pityka/saddle</url>
      <developers>
        <developer>
          <id>adamklein</id>
          <name>Adam Klein</name>
          <url>http://blog.adamdklein.com</url>
        </developer>
        <developer>
          <id>chrislewis</id>
          <name>Chris Lewis</name>
          <email>chris@thegodcode.net</email>
          <url>http://www.thegodcode.net/</url>
          <organizationUrl>https://www.novus.com/</organizationUrl>
          <timezone>-5</timezone>
        </developer>
        <developer>
          <id>pityka</id>
          <name>Istvan Bartha</name>
        </developer>
      </developers>
  },
  fork := true,
  Global / cancelable := true,
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[ReversedMissingMethodProblem](
      "*"
    ),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.stats.*"),
    ProblemFilters.exclude[MissingClassProblem]("org.saddle.ops.macroImpl.*")
  )
)

lazy val specs = List(
  ("org.specs2" %% "specs2-core" % "4.14.1-cross" % "test"),
  ("org.specs2" %% "specs2-scalacheck" % "4.14.1-cross" % "test")
)

lazy val scalaTest = List(
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("saddle-core"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-core",
    mimaBinaryIssueFilters := Seq(
      // format: off
      // private
      ProblemFilters.exclude[Problem]("org.saddle.vec.VecImpl*"),
      // Vec has package private methods, which prevent user extensions of it.
      ProblemFilters.exclude[ReversedMissingMethodProblem]("org.saddle.Vec.*"),
      // binary incompat in static methods, only usable in java
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.saddle.scalar.ScalarTagByte.missing"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.saddle.scalar.ScalarTagChar.missing"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.saddle.scalar.ScalarTagFloat.missing"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.saddle.scalar.ScalarTagShort.missing"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.saddle.scalar.ScalarTagFloat.isMissing"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.saddle.scalar.ScalarTagBool.notMissing"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.saddle.scalar.ScalarTagInt.notMissing"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.saddle.scalar.ScalarTagLong.notMissing"),
      // These helper methods should not be part of public api
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.csv.CsvParser.readFile"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.csv.CsvParser.readFile"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.readMatDataFromChannel"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.readHeaderFromChannel"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.readFully"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.sequence"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.parse"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.parseHeader"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.parseHeader"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.parse"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.sequence"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.readFully"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.readHeaderFromChannel"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.npy.Reader.readMatDataFromChannel")
      // format: on
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-kernel" % "2.6.1"
    ) ++ specs ++ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Nil
      case Some((2, _)) =>
        List("org.scala-lang.modules" %% "scala-collection-compat" % "2.7.0")
    })
  )
  .jsSettings(
    fork := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.6.1",
      "org.specs2" %%% "specs2-core" % "4.20.3" % "test",
      "org.specs2" %%% "specs2-scalacheck" % "4.20.3" % "test"
    ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Nil
      case Some((2, _)) =>
        List("org.scala-lang.modules" %%% "scala-collection-compat" % "2.7.0")
    })
  )
  .dependsOn(spire, io)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val coreJVMTests = project
  .in(file("saddle-core-jvm-test"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-core-jvm-test",
    publishArtifact := false,
    publish / skip := true
  )
  .settings(
    libraryDependencies ++= specs ++ scalaTest
  )
  .dependsOn(coreJVM, binary)

lazy val inlinedOpsMacroImpl = project
  .in(file("saddle-ops-inlined-macroimpl"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-ops-inlined-macroimpl",
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Nil
        case Some((2, _)) =>
          Seq(
            "org.scala-lang" % "scala-reflect" % scalaVersion.value
          )
      }) ++ specs
  )
  .dependsOn(coreJVM)
lazy val inlinedOps = project
  .in(file("saddle-ops-inlined"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-ops-inlined",
    libraryDependencies ++= specs
  )
  .dependsOn(coreJVM % "compile->compile;test->test", inlinedOpsMacroImpl)

lazy val bench =
  project
    .in(file("saddle-jmh"))
    .settings(commonSettings: _*)
    .settings(publish / skip := true)
    .dependsOn(coreJVM, inlinedOps, linalg)
    .enablePlugins(JmhPlugin)

lazy val time = project
  .in(file("saddle-time"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-time",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.7.0",
      "joda-time" % "joda-time" % "2.1",
      "org.joda" % "joda-convert" % "1.2",
      "org.scala-saddle" % "google-rfc-2445" % "20110304"
    ) ++ specs
  )
  .dependsOn(coreJVM)

lazy val stats = project
  .in(file("saddle-stats"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-stats",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math" % "2.2" % "test"
    ) ++ specs
  )
  .dependsOn(coreJVM)

lazy val linalg = project
  .in(file("saddle-linalg"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-linalg",
    libraryDependencies ++= Seq(
      "io.github.pityka" % "netlib-java" % "0.1.0"
    ) ++ scalaTest
  )
  .dependsOn(
    coreJVM,
    inlinedOps
  )

lazy val binary = project
  .in(file("saddle-binary"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-binary"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ujson" % "1.4.2"
    ) ++ scalaTest,
    mimaBinaryIssueFilters := Seq(
      // format: off      
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Reader.sequence"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Reader.readMatDataFromChannel"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Reader.readHeaderFromChannel"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.writeFully"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.createHeader"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.createFrameDescriptor"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.createMatDescriptor"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.KEY_rowix"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.KEY_colix"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.KEY_v"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.KEY_rowmajor"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.KEY_numrows"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.KEY_numcols"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.KEY_datatype"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.createMatDescriptor"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.createFrameDescriptor"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.createHeader"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.writeFully"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.saddle.binary.Reader#ByteChannel.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Writer.put"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Reader.readFrameDataFromChannel"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.binary.Reader.readFully")
      // format: on
    )
  )
  .dependsOn(coreJVM)

lazy val circe = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("saddle-circe"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-circe"
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.1"
    ) ++ scalaTest
  )
  .jsSettings(
    fork := false,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % "0.14.1",
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test"
    )
  )
  .dependsOn(core)
lazy val jsoniter = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("saddle-jsoniter"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-jsoniter",
    versionPolicyCheck / skip := true,
    versionCheck / skip := true
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.13.10",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.13.10" % "compile-internal",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.13.10" % "test"
    ) ++ scalaTest
  )
  .jsSettings(
    fork := false,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.13.10",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.13.10" % "compile-internal",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.13.10" % "test",
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test"
    )
  )
  .dependsOn(core)

lazy val circeJS = circe.js

lazy val circeJVM = circe.jvm

lazy val spire = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("spire-prng"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-spire-prng",
    scalaVersion := scalaVersionInBuild
  )
  .jsSettings(
    fork := false
  )

lazy val spireJVM = spire.jvm

lazy val spireJS = spire.js

lazy val io = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("saddle-io"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-io",
    scalaVersion := scalaVersionInBuild,
    libraryDependencies ++= scalaTest ++ specs,
    mimaBinaryIssueFilters := Seq(
      // format: off
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.saddle.io.npy.package.readFully"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.saddle.io.npy.package.readFully")
      // format: on
    )
  )
  .jsSettings(
    fork := false
  )

lazy val docs = project
  .in(file("saddle-docs"))
  .dependsOn(coreJVM, linalg, circeJVM, binary, inlinedOps)
  .settings(
    scalaVersion := scalaVersionInBuild,
    crossScalaVersions := Nil,
    scalacOptions ++= Seq(
      "-language:postfixOps"
    ),
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      (inAnyProject -- inProjects(
        coreJS,
        circeJS,
        spireJS,
        io.js,
        jsoniter.js,
        bench,
        inlinedOpsMacroImpl,
        stats,
        time
      )),
    publish / skip := true,
    publishArtifact := false,
    moduleName := "saddle-docs",
    mdocVariables := Map(
      "VERSION" -> version.value
    ),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value
  )
  .enablePlugins(MdocPlugin, ScalaUnidocPlugin)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    crossScalaVersions := Nil,
    publishArtifact := false,
    publish / skip := true
  )
  .aggregate(
    coreJVM,
    coreJS,
    coreJVMTests,
    time,
    stats,
    linalg,
    binary,
    circeJS,
    circeJVM,
    jsoniter.js,
    jsoniter.jvm,
    inlinedOps,
    spireJVM,
    spireJS,
    io.jvm,
    io.js,
    inlinedOpsMacroImpl,
    inlinedOps
  )

ThisBuild / parallelExecution := false
