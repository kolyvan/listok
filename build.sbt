name := "listok"

version := "0.2.0"

organization := "ru.listok"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"  % "test"

libraryDependencies += "org.scala-tools.sbinary" %% "sbinary" % "0.4.0"

// Use the name and version to define the jar name.
//jarName <<= (name, version) {
//	(name: String, version: String) => name + "-" + version + ".jar"
//}

maxErrors := 20

pollInterval := 1000

javacOptions ++= Seq("-source", "1.5", "-target", "1.5")

scalacOptions ++= Seq("-deprecation", "-unchecked")

scalaVersion := "2.9.0"

// define the statements initially evaluated when entering 'console', 'console-quick', or 'console-project'
initialCommands := """
  import System.{currentTimeMillis => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
"""

// set the initial commands when entering 'console' only
initialCommands in console := "import ru.listok._"

// set the main class for packaging the main jar
// 'run' will still auto-detect and prompt
// change Compile to Test to set it for the test jar
mainClass in (Compile, packageBin) := Some("ru.listok.main")

// set the main class for the main 'run' task
// change Compile to Test to set it for 'test:run'
mainClass in (Compile, run) := Some("ru.listok.main")

// add <base>/input to the files that '~' triggers on
watchSources <+= baseDirectory map { _ / "input" }

// add a maven-style repository
//resolvers += "name" at "url"

// add a sequence of maven-style repositories
//resolvers ++= Seq("name" at "url")

// define the repository to publish to
//publishTo := Some("name" at "url")

// set Ivy logging to be at the highest level
ivyLoggingLevel := UpdateLogging.Full

// disable updating dynamic revisions (including -SNAPSHOT versions)
offline := true

// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// set the prompt (for the current project) to include the username
// shellPrompt := { state => System.getProperty("user.name") + "> " }
// shellPrompt := "sbt> " 

// disable printing timing information, but still print [success]
// showTiming := false
showTiming := true

// disable printing a message indicating the success or failure of running a task
// showSuccess := false
showSuccess := true

// change the format used for printing task completion time
timingFormat := {
	import java.text.DateFormat
	DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT)
}

// disable using the Scala version in output paths and artifacts
//crossPaths := false
crossPaths := true

// fork a new JVM for 'run' and 'test:run'
fork := true

// fork a new JVM for 'test:run', but not 'run'
fork in Test := true

// add a JVM option to use when forking a JVM for 'run'
//javaOptions += "-Xmx2G"
javaOptions += "-Xmx1G"

// only use a single thread for building
parallelExecution := false

// Execute tests in the current project serially
//   Tests from other projects may still run concurrently.
parallelExecution in Test := true

// set the location of the JDK to use for compiling Java code.
// if 'fork' is true, this is used for 'run' as well
javaHome := Some(file("/usr/local/jdk1.6.0"))

// Use Scala from a directory on the filesystem instead of retrieving from a repository
scalaHome := Some(file("/usr/local/share/scala-2.9.0.1"))

// don't aggregate clean (See FullConfiguration for aggregation details)
aggregate in clean := false

// only show warnings and errors on the screen for compilations.
//  this applies to both test:compile and compile and is Info by default
logLevel in compile := Level.Warn

// only show warnings and errors on the screen for all tasks (the default is Info)
//  individual tasks can then be more verbose using the previous setting
logLevel := Level.Warn

// only store messages at info and above (the default is Debug)
//   this is the logging level for replaying logging with 'last'
persistLogLevel := Level.Debug

// only show 10 lines of stack traces
traceLevel := 10

// only show stack traces up to the first sbt stack frame
traceLevel := 0

// add SWT to the unmanaged classpath
//unmanagedJars in Compile += file("/usr/share/java/swt.jar")

// publish test jar, sources, and docs
publishArtifact in Test := true

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// change the classifier for the docs artifact
artifactClassifier in packageDoc := Some("doc")

// Copy all managed dependencies to <build-root>/lib_managed/
//   This is essentially a project-local cache and is different
//   from the lib_managed/ in sbt 0.7.x.  There is only one
//   lib_managed/ in the build root (not per-project).
retrieveManaged := true


