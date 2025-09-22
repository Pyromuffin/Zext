
scalaVersion := "3.7.3"
isSnapshot := true


val org = "com.pyromuffin"

/*
lazy val meta = (project in file("meta"))
  .settings(
      name := "Zext-meta",
      scalaVersion := "2.13.16",
      isSnapshot := true,
      version := "0.1.1",
      organization := org,
      libraryDependencies += "org.scalameta" %% "scalameta" % "4.12.7",

  )
*/
resolvers += "Zobjectifier packages" at "https://maven.pkg.github.com/pyromuffin/zobjectifier"

githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_TOKEN")

lazy val zext = (project in file("."))
  .settings(
    name := "Zext",

    version := "0.2.0",
    organization := org,
    autoCompilerPlugins := true,
    addCompilerPlugin("com.pyromuffin" %% "zobjectifier" % "1.2.2"),

    libraryDependencies += "com.pyromuffin" %% "zobjectifier" % "1.2.2",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.18.0",
    libraryDependencies += "org.apache.opennlp" % "opennlp-tools" % "2.5.5",
    libraryDependencies += "org.apache.opennlp" % "opennlp-tools-models" % "2.5.5",
    libraryDependencies += "org.apache.opennlp" % "opennlp-models-pos-en" % "1.3.0",
    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.17",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17",
    githubOwner := "pyromuffin",
    githubRepository := "zext",

  )

Compile / scalacOptions ++= Seq("-Xplugin-require:Zobjectifier", "-Wconf:msg=Non local returns:s", "-experimental")


/*
enablePlugins(ScalaNativePlugin)

import scala.scalanative.build._
nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.commix)
}
*/
