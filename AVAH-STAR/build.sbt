val sparkVersion = "2.4.7"
//scalaVersion := "2.12.8" // For ADAM-Cannoli/Spark3
scalaVersion := "2.11.12" // For GATK/Spark2

version := "0.1"
name := "avah_" + sparkVersion

// assemblyJarName in assembly := "avah-deploy.jar"

//assemblyMergeStrategy in assembly := {
//  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
//  case x => MergeStrategy.first
//}

sparkVersion match {
  case "3.0.0" =>
    libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.0.0";
    libraryDependencies += "org.apache.spark" %% "spark-repl" % "3.0.0";
  case "2.4.7" =>
    libraryDependencies += "org.apache.spark" %% "spark-sql" % "2.4.7";
    libraryDependencies += "org.apache.spark" %% "spark-repl" % "2.4.7";
}