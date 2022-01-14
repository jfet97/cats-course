val scala3Version = "3.1.0"
val catsVersion = "2.7.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-course",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion
    )
  )