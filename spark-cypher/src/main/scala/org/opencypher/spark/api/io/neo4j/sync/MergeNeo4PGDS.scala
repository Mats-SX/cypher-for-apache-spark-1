/*
 * Copyright (c) 2016-2018 "Neo4j Sweden, AB" [https://neo4j.com]
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
package org.opencypher.spark.api.io.neo4j.sync

import java.util.concurrent.Executors

import org.apache.logging.log4j.scala.Logging
import org.apache.spark.sql.Row
import org.neo4j.driver.internal.value.ListValue
import org.neo4j.driver.v1.{Value, Values}
import org.opencypher.okapi.api.graph.PropertyGraph
import org.opencypher.okapi.api.types.{CTNode, CTRelationship}
import org.opencypher.okapi.ir.api.expr.{EndNode, Property, StartNode}
import org.opencypher.okapi.neo4j.io.Neo4jHelpers.Neo4jDefaults._
import org.opencypher.okapi.neo4j.io.Neo4jHelpers._
import org.opencypher.okapi.neo4j.io.{EntityWriter, Neo4jConfig}
import org.opencypher.spark.api.CAPSSession
import org.opencypher.spark.impl.CAPSConverters._
import org.opencypher.spark.impl.CAPSRecords

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}

case class EntityKeys(
  nodeKeys: Map[Set[String], Set[String]],
  relKeys: Map[String, Set[String]]
)

object Neo4jSync extends Logging {

  def merge(graph: PropertyGraph, config: Neo4jConfig, entityKeys: EntityKeys)
    (implicit caps: CAPSSession): Unit = {
    val executorCount = caps.sparkSession.sparkContext.statusTracker.getExecutorInfos.length
    implicit val executionContext: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(executorCount))

    logger.debug(s"Using $executorCount Threads")

    // TODO: Create constraints if they don't exist already
    //    config.withSession { session =>
    //      logger.info(s"Creating database uniqueness constraint on entity keys")
    //      session.run(s"CREATE CONSTRAINT ON (n:$metaLabel) ASSERT n.$metaPropertyKey IS UNIQUE").consume()
    //    }

    val writesCompleted = for {
      _ <- Future.sequence(MergeWriters.writeNodes(graph, config, entityKeys.nodeKeys))
      _ <- Future.sequence(MergeWriters.writeRelationships(graph, config, entityKeys.relKeys))
      _ <- Future {
        config.withSession { session =>
          session.run(s"MATCH (n) REMOVE n.$metaPropertyKey")
        }
      }
    } yield Future {}
    Await.result(writesCompleted, Duration.Inf)
  }
}

case object MergeWriters {
  def writeNodes(graph: PropertyGraph, config: Neo4jConfig, nodeKeys: Map[Set[String], Set[String]])
    (implicit caps: CAPSSession): Set[Future[Unit]] = {
    val result: Set[Future[Unit]] = graph.schema.labelCombinations.combos.map { combo =>
      val nodeScan = graph.nodes("n", CTNode(combo), exactLabelMatch = true).asCaps
      val mapping = computeMapping(nodeScan, includeId = true)
      nodeScan
        .df
        .rdd
        .foreachPartitionAsync(i => EntityWriter.mergeNodes(i, mapping, config, combo, nodeKeys(combo))(rowToListValue))
    }
    result
  }

  def writeRelationships(graph: PropertyGraph, config: Neo4jConfig, relKeys: Map[String, Set[String]])
    (implicit caps: CAPSSession): Set[Future[Unit]] = {
    graph.schema.relationshipTypes.map { relType =>
      val relScan = graph.relationships("r", CTRelationship(relType)).asCaps
      val mapping = computeMapping(relScan, includeId = false)

      val header = relScan.header
      val relVar = header.entityVars.head
      val startExpr = header.expressionsFor(relVar).collect { case s: StartNode => s }.head
      val endExpr = header.expressionsFor(relVar).collect { case e: EndNode => e }.head
      val startColumn = relScan.header.column(startExpr)
      val endColumn = relScan.header.column(endExpr)
      val startIndex = relScan.df.columns.indexOf(startColumn)
      val endIndex = relScan.df.columns.indexOf(endColumn)

      relScan
        .df
        .rdd
        .foreachPartitionAsync(i =>
          EntityWriter.mergeRelationships(
            i,
            startIndex,
            endIndex,
            mapping,
            config,
            relType,
            relKeys(relType)
          )(rowToListValue)
        )
    }
  }

  private def rowToListValue(row: Row): ListValue = {
    val array = new Array[Value](row.size)
    var i = 0
    while (i < row.size) {
      array(i) = Values.value(row.get(i))
      i += 1
    }
    new ListValue(array: _*)
  }

  private def computeMapping(entityScan: CAPSRecords, includeId: Boolean): Array[String] = {
    val header = entityScan.header
    val nodeVar = header.entityVars.head
    val properties: Set[Property] = header.expressionsFor(nodeVar).collect {
      case p: Property => p
    }

    val columns = entityScan.df.columns
    val mapping = Array.fill[String](columns.length)(null)

    if (includeId) {
      val idIndex = columns.indexOf(header.column(nodeVar))
      mapping(idIndex) = metaPropertyKey
    }

    properties.foreach { property =>
      val index = columns.indexOf(header.column(property))
      mapping(index) = property.key.name
    }

    mapping
  }
}
