package org.opencypher.caps

import org.opencypher.caps.Result.Result
import org.opencypher.caps.api.spark.CAPSGraph
import org.opencypher.caps.api.value.CypherValue

case class GraphImpl(graph: CAPSGraph) extends Graph {
  override def cypher(cypher: String, params: Map[String, Any]): Result = {
//    if (createStatement)
    // cheat

    val internalResult = graph.cypher(cypher, params.mapValues(CypherValue(_)))

    internalResult.records.toLocalScalaIterator.map { cypherMap =>
      cypherMap.keys.map(k => k -> cypherMap.get(k).orNull.toString).toMap
    }.toList
  }
}
