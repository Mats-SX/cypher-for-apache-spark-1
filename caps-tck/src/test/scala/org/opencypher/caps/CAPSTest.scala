package org.opencypher.caps

import org.junit.jupiter.api.{DynamicTest, TestFactory}
import org.opencypher.caps.TCKAdapterForCAPS.AsTckGraph
import org.opencypher.caps.api.spark.CAPSSession
import org.opencypher.caps.test.support.TestGraph

import scala.collection.JavaConverters._

class CAPSTest {

  val tckScenarios = CypherTCK.allTckScenarios
  val ourScenarios = CypherTCK.parseScenarios("./src/test/scala/org/opencypher/caps/")
  val allScenarios = tckScenarios ++ ourScenarios

  @TestFactory
  def testGraphTests() = {
    implicit val caps = CAPSSession.local()
    def createTestGraph(): Graph = TestGraph().graph

    val dynamicTests = allScenarios.map { scenario =>
      val name = scenario.toString
      val executable = scenario(createTestGraph)
      DynamicTest.dynamicTest(name, executable)
    }
    dynamicTests.asJavaCollection
  }

}
