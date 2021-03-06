/*
 * Copyright (c) 2016-2018 "Neo4j, Inc." [https://neo4j.com]
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Attribution Notice under the terms of the Apache License 2.0
 *
 * This work was created by the collective efforts of the openCypher community.
 * Without limiting the terms of Section 6, any Derivative Work that is not
 * approved by the public consensus process of the openCypher Implementers Group
 * should not be described as “Cypher” (and Cypher® is a registered trademark of
 * Neo4j Inc.) or as "openCypher". Extensions by implementers or prototypes or
 * proposals for change that have been documented or implemented should only be
 * described as "implementation extensions to Cypher" or as "proposed changes to
 * Cypher that are not yet approved by the openCypher community".
 */
package org.opencypher.spark.examples

import org.opencypher.okapi.api.graph.{GraphName, Namespace, QualifiedGraphName}
import org.opencypher.spark.api.CAPSSession
import org.opencypher.spark.api.io.file.FileCsvGraphDataSource
import org.opencypher.spark.api.io.neo4j.CommunityNeo4jGraphDataSource._
import org.opencypher.spark.api.io.neo4j.{Neo4jConfig, CommunityNeo4jGraphDataSource}
import org.opencypher.spark.examples.Neo4jHelpers._

/**
  * Demonstrates connecting a graph from a CSV data source with a graph from a Neo4j data source.
  *
  * Writes updates back to the Neo4j database with Cypher queries.
  */
object Neo4jWorkflow extends App {
  // Create CAPS session
  implicit val session: CAPSSession = CAPSSession.local()

  // Start a Neo4j instance and populate it with social network data
  val neo4j = Neo4jHelpers.startNeo4j(personNetwork)

  // Register Graph Data Sources (GDS)
  session.registerSource(Namespace("socialNetwork"), CommunityNeo4jGraphDataSource(neo4j.dataSourceConfig))

  // Access the graph via its qualified graph name
  val socialNetwork = session.graph("socialNetwork.graph")

  // Register a File-based data source in the Cypher session
  session.registerSource(Namespace("csv"), FileCsvGraphDataSource(rootPath = getClass.getResource("/csv").getFile))


  // Access the graph via its qualified graph name
  val purchaseNetwork = session.graph("csv.products")

  // Build new recommendation graph that connects the social and product graphs and
  // create new edges between users and customers with the same name
  val recommendationGraph = session.cypher(
    """|FROM GRAPH socialNetwork.graph
       |MATCH (p:Person)
       |FROM GRAPH csv.products
       |MATCH (c:Customer)
       |WHERE p.name = c.name
       |CONSTRUCT
       |  ON socialNetwork.graph, csv.products
       |  NEW (p)-[:IS]->(c)
       |RETURN GRAPH
    """.stripMargin
  ).graph.get

  // Query for product recommendations
  val recommendations = recommendationGraph.cypher(
    """|MATCH (person:Person)-[:FRIEND_OF]-(friend:Person),
       |(friend)-[:IS]->(customer:Customer),
       |(customer)-[:BOUGHT]->(product:Product)
       |RETURN person.name AS for, collect(DISTINCT product.title) AS recommendations""".stripMargin)

  // Use Cypher queries to write the product recommendations back to Neo4j
  recommendations.getRecords.collect.foreach { recommendation =>
    neo4j.execute(
      s"""|MATCH (p:Person {name: ${recommendation.get("for").get.toCypherString}})
          |SET p.should_buy = ${recommendation.get("recommendations").get.toCypherString}""".stripMargin)
  }

  // Proof that the write-back to Neo4j worked, retrieve and print updated Neo4j results
  val updatedNeo4jSource = CommunityNeo4jGraphDataSource(neo4j.dataSourceConfig)
  session.registerSource(Namespace("updated-neo4j"), updatedNeo4jSource)
  val socialNetworkWithRanks = session.graph(QualifiedGraphName(Namespace("updated-neo4j"), neo4jDefaultGraphName))
  socialNetworkWithRanks.cypher("MATCH (p) WHERE p.should_buy IS NOT NULL RETURN p.name, p.should_buy").show

  // Shutdown Neo4j test instance
  neo4j.stop()

  def personNetwork =
    s"""|CREATE (a:Person { name: 'Alice', age: 10 })
        |CREATE (b:Person { name: 'Bob', age: 20})
        |CREATE (c:Person { name: 'Carol', age: 15})
        |CREATE (a)-[:FRIEND_OF { since: '23/01/1987' }]->(b)
        |CREATE (b)-[:FRIEND_OF { since: '12/12/2009' }]->(c)""".stripMargin
}
