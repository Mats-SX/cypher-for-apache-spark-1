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
 */
package org.opencypher.okapi.impl.spark

import org.opencypher.okapi.api.graph.PropertyGraph
import org.opencypher.okapi.api.table.CypherRecords
import org.opencypher.okapi.api.value.CypherValue._
import org.opencypher.okapi.ir.api.expr.{Expr, Var}

trait CAPSSessionOps {

  def filter(
      graph: PropertyGraph,
      in: CypherRecords,
      expr: Expr,
      queryParameters: CypherMap): CypherRecords

  def select(
      graph: PropertyGraph,
      in: CypherRecords,
      fields: IndexedSeq[Var],
      queryParameters: CypherMap): CypherRecords

  def project(
      graph: PropertyGraph,
      in: CypherRecords,
      expr: Expr,
      queryParameters: CypherMap): CypherRecords

  def alias(
      graph: PropertyGraph,
      in: CypherRecords,
      alias: (Expr, Var),
      queryParameters: CypherMap): CypherRecords

}