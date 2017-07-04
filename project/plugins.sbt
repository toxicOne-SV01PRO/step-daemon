resolvers += "spray repo" at "http://repo.spray.io"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.17")

addSbtPlugin("com.lihaoyi" % "workbench" % "0.2.3")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0")