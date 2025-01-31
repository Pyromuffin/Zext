
scalaVersion := "3.6.3"
isSnapshot := true

resolvers += "Zobjectifier packages" at "https://maven.pkg.github.com/pyromuffin/zobjectifier"


githubTokenSource := TokenSource.Or(
    TokenSource.Environment("GITHUB_TOKEN"), // Injected during a github workflow for publishing
    TokenSource.GitConfig("github.token") // local token set in ~/.gitconfig
)

credentials += Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "_",
    sys.env("GITHUB_TOKEN")
)


val org = "com.pyromuffin"

lazy val meta = (project in file("meta"))
  .settings(
      name := "Zext-meta",
      scalaVersion := "2.13.16",
      isSnapshot := true,
      version := "0.1.1",
      organization := org,
      libraryDependencies += "org.scalameta" %% "scalameta" % "4.12.7",

  )

lazy val zext = (project in file("."))
  .settings(
    name := "Zext",

    version := "0.1.9",
    organization := org,
    autoCompilerPlugins := true,
    addCompilerPlugin("com.pyromuffin" %% "zobjectifier" % "1.2.0"),

    libraryDependencies += "com.pyromuffin" %% "zobjectifier" % "1.2.0",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.17.0",
    libraryDependencies += "org.apache.opennlp" % "opennlp-tools" % "2.5.3",
    libraryDependencies += "org.apache.opennlp" % "opennlp-tools-models" % "2.5.3",
    libraryDependencies += "org.apache.opennlp" % "opennlp-models-pos-en" % "1.2.0",
    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.16",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.16",
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
