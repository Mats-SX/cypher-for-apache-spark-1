package org.opencypher.spark

import java.util.UUID

import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.opencypher.spark.benchmark.Configuration.{Logging, Neo4jAddress, Neo4jPassword, Neo4jUser}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

object TestSession {

  lazy val default = TestSessionFactory.create


  trait Fixture extends BeforeAndAfterEach {
    self: FunSuite =>

    implicit val session = TestSession.default
    implicit val factory = PropertyGraphFactory.create

    override protected def beforeEach(): Unit = {
      factory.clear()
    }
  }
}

object TestSessionFactory {
  def create = {
    val conf = new SparkConf(true)
    conf.set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    conf.set("spark.kryo.registrator","org.opencypher.spark.CypherKryoRegistrar")
    conf.set("spark.neo4j.bolt.password", Neo4jPassword.get())
    conf.set("spark.neo4j.bolt.user", Neo4jUser.get())
    conf.set("spark.neo4j.bolt.url", Neo4jAddress.get())

    //
    // This may or may not help - depending on the query
    // conf.set("spark.kryo.referenceTracking","false")

    //
    // Enable to see if we cover enough
    // conf.set("spark.kryo.registrationRequired", "true")

    //
    // If this is slow, you might be hitting: http://bugs.java.com/view_bug.do?bug_id=8077102
    //
    val session = SparkSession
      .builder()
      .config(conf)
      .master("local[*]")
      .appName(s"cypher-for-apache-spark-tests-${UUID.randomUUID()}")
      .getOrCreate()

    session.sparkContext.setLogLevel(Logging.get())
    session
  }
}
