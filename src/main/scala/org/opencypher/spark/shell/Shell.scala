package org.opencypher.spark.shell

import ammonite.util.Bind._
import ammonite.util.Util
import org.opencypher.spark.CypherForApacheSpark
import org.opencypher.spark.benchmark.RunBenchmark

object Shell {

  def main(args: Array[String]): Unit = {
    implicit val session = RunBenchmark.sparkSession
    try {
      val welcomeBanner = {
        val ownVersion = CypherForApacheSpark.version.getOrElse("<unknown>")
        val ammoniteVersion = ammonite.Constants.version
        val scalaVersion = scala.util.Properties.versionNumberString
        val javaVersion = System.getProperty("java.version")
        val sparkVersion = session.version
        Util.normalizeNewlines(
          """  _____          __             ___
            = / ___/_ _____  / /  ___ ____  / _/__  ____
            =/ /__/ // / _ \/ _ \/ -_) __/ / _/ _ \/ __/
            =\___/\_, / .__/_//_/\__/_/   /_/ \___/_/
            =   _/___/_/            __         ____              __
            =  / _ | ___  ___ _____/ /  ___   / __/__  ___ _____/ /__
            = / __ |/ _ \/ _ `/ __/ _ \/ -_) _\ \/ _ \/ _ `/ __/  '_/
            =/_/ |_/ .__/\_,_/\__/_//_/\__/ /___/ .__/\_,_/_/ /_/\_\
            =     /_/                          /_/
            =""".stripMargin('=') +
          s"""|
              |Version $ownVersion
              |(Apache Spark $sparkVersion, Scala $scalaVersion, Java $javaVersion, Ammonite $ammoniteVersion)
              |
              |Cypher is a registered trademark of Neo Technology, Inc.
              |
           """.stripMargin
        )
      }
      val frontend = if (System.getProperty("os.name").startsWith("Windows")) "JLineWindows" else "JLineUnix"

//      import org.opencypher.spark
//      import org.opencypher.spark._
//      import org.opencypher.spark.api._
//      import org.opencypher.spark.api.implicits._
//      import org.opencypher.spark.api.types._
      val repl = new ammonite.Main(
        welcomeBanner = Some(welcomeBanner),
        predef =
          s"""|repl.frontEnd() = ammonite.frontend.FrontEnd.$frontend
              |repl.prompt() = \"(:Cypher)-[:FOR]->(:ApacheSpark) \"
              |import org.opencypher.spark.prototype.PrototypeDemo2._
              |import org.opencypher.spark.prototype.impl.instances.spark.cypher._
              |import org.opencypher.spark.prototype.impl.syntax.cypher._
              |""".stripMargin
      ).instantiateRepl(Seq("session" -> session))
      repl.run()
    } finally {
      session.stop()
    }

    // Needed; otherwise the shell hangs on exit
    System.exit(0)
  }
}
