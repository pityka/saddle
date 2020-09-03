import com.typesafe.tools.mima.core._

lazy val scalaTestVersion = "3.2.2"
lazy val scalaVersionInBuild = "2.12.12"

lazy val commonSettings = Seq(
  scalaVersion := scalaVersionInBuild,
  parallelExecution in Test := false,
  scalacOptions ++= Seq(
    "-opt:l:method",
    "-opt:l:inline",
    "-opt-inline-from:org.saddle.**",
    "-opt-warnings",
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:postfixOps",
    "-language:existentials",
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    // "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    // "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
    "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates" // Warn if a private member is unused.
  ),
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Xfatal-warnings"))
) ++ Seq(
  organization := "io.github.pityka",
  licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
  pomExtra in Global := {
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
  cancelable in Global := true,
  mimaPreviousArtifacts := previousStableVersion.value
    .map(organization.value %% name.value % _)
    .toSet,
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[ReversedMissingMethodProblem](
      "org.saddle.Vec.zipMapIdx"
    )
  )
)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("saddle-core"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-core"
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-kernel" % "2.2.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.specs2" %% "specs2-core" % "4.10.3" % "test",
      "org.specs2" %% "specs2-scalacheck" % "4.10.3" % "test"
    )
  )
  .jsSettings(
    fork := false,
    coverageEnabled := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.2.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.specs2" %%% "specs2-core" % "4.9.4" % "test",
      "org.specs2" %%% "specs2-scalacheck" % "4.9.4" % "test"
    )
  )
  .dependsOn(spire)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val coreJVMTests = project
  .in(file("saddle-core-jvm-test"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-core-jvm-test",
    publishArtifact := false,
    skip in publish := true
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.2.0",
      "org.specs2" %% "specs2-core" % "4.10.3" % "test",
      "org.specs2" %% "specs2-scalacheck" % "4.10.3" % "test",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
    )
  )
  .dependsOn(coreJVM, binary)

lazy val inlinedOps = project
  .in(file("saddle-ops-inlined"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-ops-inlined",
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.10.3" % "test",
      "org.specs2" %% "specs2-scalacheck" % "4.10.3" % "test"
    )
  )
  .dependsOn(coreJVM % "compile->compile;test->test")

lazy val bench =
  project
    .in(file("saddle-jmh"))
    .settings(commonSettings: _*)
    .settings(skip in publish := true)
    .dependsOn(coreJVM, inlinedOps, linalg)
    .enablePlugins(JmhPlugin)

lazy val time = project
  .in(file("saddle-time"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-time",
    libraryDependencies ++= Seq(
      "joda-time" % "joda-time" % "2.1",
      "org.joda" % "joda-convert" % "1.2",
      "org.scala-saddle" % "google-rfc-2445" % "20110304",
      "org.specs2" %% "specs2-core" % "4.10.3" % "test",
      "org.specs2" %% "specs2-scalacheck" % "4.10.3" % "test"
    )
  )
  .dependsOn(coreJVM)

lazy val stats = project
  .in(file("saddle-stats"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-stats",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math" % "2.2" % "test",
      "org.specs2" %% "specs2-core" % "4.10.3" % "test",
      "org.specs2" %% "specs2-scalacheck" % "4.10.3" % "test"
    )
  )
  .dependsOn(coreJVM)

lazy val linalg = project
  .in(file("saddle-linalg"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-linalg",
    libraryDependencies ++= Seq(
      "com.github.fommil.netlib" % "all" % "1.1.2" pomOnly (),
      "net.sourceforge.f2j" % "arpack_combined_all" % "0.1",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
    )
  )
  .dependsOn(coreJVM, inlinedOps)

lazy val binary = project
  .in(file("saddle-binary"))
  .settings(commonSettings: _*)
  .settings(
    name := "saddle-binary"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "ujson" % "1.2.0",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
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
      "io.circe" %% "circe-core" % "0.13.0",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
    )
  )
  .jsSettings(
    fork := false,
    coverageEnabled := false,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % "0.13.0",
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
    coverageEnabled := false,
    fork := false
  )

lazy val spireJVM = spire.jvm

lazy val spireJS = spire.js

lazy val docs = project
  .in(file("saddle-docs"))
  .dependsOn(coreJVM, linalg, circeJVM, binary)
  .settings(
    unidocProjectFilter in (ScalaUnidoc, unidoc) :=
      (inAnyProject -- inProjects(coreJS, circeJS, spireJS)),
    publishArtifact := false,
    moduleName := "saddle-docs",
    mdocVariables := Map(
      "VERSION" -> version.value
    ),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value
  )
  .enablePlugins(MdocPlugin, ScalaUnidocPlugin)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false
  )
  .settings(
    git.remoteRepo := ""
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
    docs,
    inlinedOps,
    spireJVM,
    spireJS
  )

parallelExecution in ThisBuild := false
