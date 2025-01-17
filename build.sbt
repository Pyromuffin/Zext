
scalaVersion := "3.2.2"
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

lazy val zext = (project in file("."))
  .settings(
    name := "Zext",

    version := "0.1.9",
    organization := org,
    autoCompilerPlugins := true,
    addCompilerPlugin("com.pyromuffin" %% "zobjectifier" % "1.0.6"),

    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.14.0",
    githubOwner := "pyromuffin",
    githubRepository := "zext",


)

Compile / scalacOptions ++= Seq("-Xplugin-require:Zobjectifier")


/*
enablePlugins(ScalaNativePlugin)

import scala.scalanative.build._
nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.commix)
}
*/
