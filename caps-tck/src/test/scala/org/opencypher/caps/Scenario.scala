package org.opencypher.caps

import org.junit.jupiter.api.function.Executable
import org.opencypher.caps.Graph.emptyRecords

case class Feature(scenarios: Seq[Scenario])

trait Step

case object NoSideEffects extends Step

case class Execute(query: String) extends Step

case class Expect(expected: List[Map[String, String]]) extends Step

case class Scenario(featureName: String, name: String, steps: List[Step]) extends (Graph => Executable) {
  override def toString() = s"""Feature "$featureName": Scenario "$name""""

  override def apply(graph: Graph): Executable = new Executable {
    override def execute(): Unit = executeOnGraph(graph)
  }

  def executeOnGraph(empty: Graph): Unit = {
    steps.foldLeft(empty -> emptyRecords) {
      case ((g, _), Execute(query)) =>
        g.execute(query)
      case ((g, r), Expect(expected)) =>
        assert(r == expected, s"Got result $r, but expected $expected")
        g -> r
      case ((g, r), NoSideEffects) =>
        g -> r
      case _ => ???
    }
  }
}
