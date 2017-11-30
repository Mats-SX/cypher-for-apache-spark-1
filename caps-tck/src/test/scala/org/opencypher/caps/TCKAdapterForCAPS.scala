package org.opencypher.caps

import org.opencypher.caps.api.spark.{CAPSGraph, CAPSRecords}
import org.opencypher.caps.api.value.CypherValue

object TCKAdapterForCAPS {

  implicit class AsTckGraph(graph: CAPSGraph) extends Graph {

    override def execute(query: String, params: Map[String, Any] = Map.empty): (Graph, Records) = {
      val capsResult = graph.cypher(query, params.mapValues(CypherValue(_)))
      val tckRecords: Records = convertToTckRecords(capsResult.records)
      val tckGraph: Graph = capsResult.graphs.values.headOption.map(AsTckGraph(_)).getOrElse(this)
      (tckGraph, tckRecords)
    }

    private def convertToTckRecords(records: CAPSRecords) = {
      records.toLocalScalaIterator.map { cypherMap =>
        cypherMap.keys.map(k => k -> cypherMap.get(k).orNull.toString).toMap
      }.toList
    }

  }

}
