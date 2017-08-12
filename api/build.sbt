name := TreeBuild.NamePrefix + "api"

mainClass in Compile := Some("JettyServer")

enablePlugins(JettyPlugin)