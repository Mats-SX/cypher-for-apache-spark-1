package org.opencypher.caps

/**
  * Mutable implementations implement .cypher
  * Immutable implementations implement .execute
  *
  * An implementation will not have to implement .cypher if .execute is overridden.
  */
trait Graph {
  final type Records = List[Map[String, String]]

  def execute(query: String, params: Map[String, Any] = Map.empty): (Graph, Records) =
    this -> cypher(query, params)

  def cypher(query: String, params: Map[String, Any] = Map.empty): Records =
    throw new UnsupportedOperationException("To use the TCK, implement this method or override .execute()")
}

object Graph {
  final val emptyRecords = List.empty[Map[String, String]]
}
