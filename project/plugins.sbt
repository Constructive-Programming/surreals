addDependencyTreePlugin
addSbtPlugin("org.typelevel"  % "sbt-tpolecat"        % "0.5.1")
addSbtPlugin("org.scalameta"  % "sbt-scalafmt"        % "2.5.2")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.10.0")
addSbtPlugin("io.spray"       % "sbt-revolver"        % "0.10.0")
addSbtPlugin("com.eed3si9n"   % "sbt-buildinfo"       % "0.12.0")
addSbtPlugin("com.github.sbt" % "sbt-dynver"          % "5.0.1")
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.9.9" cross CrossVersion.full)
