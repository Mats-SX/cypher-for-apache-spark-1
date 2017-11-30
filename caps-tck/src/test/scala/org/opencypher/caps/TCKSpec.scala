package org.opencypher.caps

import gherkin.ast.GherkinDocument
import gherkin.pickles.{Compiler, PickleString, PickleTable}
import gherkin.{AstBuilder, Parser, TokenMatcher}
import org.junit.jupiter.api.function.Executable
import org.junit.runner.Runner
import org.junit.runner.notification.RunNotifier
import org.opencypher.caps.Result.Result

import scala.collection.JavaConverters._
import scala.io.Source

object CypherTCK {

  def allTckScenarios: Seq[Scenario] = {
    // parse all features bla bla

    ???
  }

  def parseScenarios(path: String): Seq[Scenario] =
    parseFeatures(path).flatMap(_.scenarios)

  def parseFeatures(path: String): Seq[Feature] = {
    val parser = new Parser[GherkinDocument](new AstBuilder)
    val matcher = new TokenMatcher

    val featureString = Source
      .fromFile(s"$path/Foo.feature")
      .getLines()
      .mkString("\n")
    val gherkinDocument = parser.parse(featureString, matcher)

    val compiler = new Compiler

    println(gherkinDocument.getFeature.getName)

    val pickle = compiler.compile(gherkinDocument).asScala.head

    val steps = pickle.getSteps.asScala.flatMap { step =>
      val scenarioStep = step.getText match {
        case "an empty graph" => // we start with empty graph implicitly
          None
        case "executing query:" =>
          require(step.getArgument.size() == 1)
          Some(ExecuteStep(step.getArgument.asScala.head.asInstanceOf[PickleString].getContent))
        case "the result should be:" =>
          require(step.getArgument.size() == 1)
          val rows = step.getArgument.asScala.head.asInstanceOf[PickleTable].getRows.asScala
          val header = rows.head
          val values = rows.tail

          val expected: List[Map[String, Any]] = values.map { v =>
            v.getCells.asScala
              .zip(header.getCells.asScala)
              .map {
                case (value, header) =>
                  header.getValue -> value.getValue
              }
              .toMap
          }.toList

          Some(ExpectStep(expected))
        case "no side effects" =>
          Some(NoSideEffectsStep)

      }

      scenarioStep
    }.toList

    Seq(new Feature {
      override def scenarios: Seq[Scenario] =
        Seq(Scenario(pickle.getName, gherkinDocument.getFeature.getName, steps))
    })
  }
}

trait Feature {
  def scenarios: Seq[Scenario]
}

trait Step {}

case object NoSideEffectsStep extends Step
//case object GivenEmptyStep extends Step
//case object GivenAnyStep extends Step

case class ExecuteStep(query: String) extends Step
case class ExpectStep(expected: Result) extends Step

case class Scenario(name: String, featureName: String, steps: List[Step]) extends (Graph => Executable) {

  self =>

  def id: String = s"$featureName : $name"

  override def toString(): String = id

  override def apply(graph: Graph): Executable = new Executable {
    override def execute(): Unit = self.execute(graph)
  }

  def execute(empty: Graph): Unit = {
    steps.foldLeft(empty -> Result.empty) {

      case ((g, _), ExecuteStep(query)) =>
        g.execute(query)
      case ((g, r), ExpectStep(expected)) =>
        assert(r == expected)
        g -> r
      case ((g, r), NoSideEffectsStep) =>
        g -> r
      case _ => ???
    }
  }
}

/**
  * Mutable implementations implement .cypher
  * Immutable implementations implement .execute
  *
  * An implementation will not have to implement .cypher if .execute is overridden.
  */
trait Graph {
  def execute(query: String, params: Map[String, Any] = Map.empty): (Graph, Result) =
    this -> cypher(query, params)

  def cypher(query: String, params: Map[String, Any] = Map.empty): Result =
    throw new UnsupportedOperationException("To use the TCK, implement this method or override .execute()")
}

object Result {
  def empty = List.empty[Map[String, Any]]

  type Result = List[Map[String, Any]]
}

trait Record {
  def get(key: String): Any
}

class TCKRunner extends Runner {
  override def run(notifier: RunNotifier) = ???

  override def getDescription = ???
}
