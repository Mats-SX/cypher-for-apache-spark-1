package org.opencypher.caps

import org.junit.jupiter.api.{DynamicTest, TestFactory}
import org.opencypher.caps.api.spark.CAPSSession
import org.opencypher.caps.test.support.TestGraph

import scala.collection.JavaConverters._

class CapsTCKTest {

//  val tckScenarios = CypherTCK.allTckScenarios
  val ourScenarios = CypherTCK.parseScenarios(
    "/Users/mats/gitRoots/sparkfork/CypherForApacheSpark/caps-tck/src/test/scala/org/opencypher/caps/"
  )

//  val allScenarios = tckScenarios ++ ourScenarios

  @TestFactory
  def testGraphTests() = {

    val empty = GraphImpl(TestGraph("")(CAPSSession.local()).graph)

    val dynamicTests = ourScenarios.map(s => {
      DynamicTest.dynamicTest(s.name, s(empty))
    })

    dynamicTests.asJavaCollection
  }
}
