lazy val common = crossProject.in(file("common"))
    .settings(name := "print-server-common")
    .settings(Build.commonSettings: _*)
    .jsSettings(Build.jsSettings: _*)
    .jvmSettings(Build.jvmSettings: _*)

lazy val commonJS = common.js

lazy val commonJVM = common.jvm

lazy val server = project.in(file("server"))
    .settings(name := "print-server-jvm")
    .settings(Build.commonSettings: _*)
    .settings(Build.jvmSettings: _*)
    .dependsOn(commonJVM)

lazy val debug = project.in(file("debug"))
    .settings(name := "print-debug-jvm")
    .settings(Build.commonSettings: _*)
    .settings(Build.jvmSettings: _*)
    .dependsOn(commonJVM, server)

Build.buildSettings
