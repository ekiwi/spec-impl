name := "spec_impl"
version := "0.1"
scalaVersion := "2.12.7"

scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11")

libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.1.+"
libraryDependencies += "edu.berkeley.cs" %% "firrtl" % "1.1.+"
libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.2.+"
libraryDependencies += "edu.berkeley.cs" %% "treadle" % "1.0.+"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
