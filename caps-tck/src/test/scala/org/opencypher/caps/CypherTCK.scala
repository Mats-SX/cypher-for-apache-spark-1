package org.opencypher.caps

import java.io.File

import gherkin.ast.GherkinDocument
import gherkin.pickles.{Compiler, Pickle, PickleString, PickleTable}
import gherkin.{AstBuilder, Parser, TokenMatcher}

import scala.collection.JavaConverters._
import scala.io.Source

object CypherTCK {

  private lazy val parser = new Parser[GherkinDocument](new AstBuilder)
  private lazy val matcher = new TokenMatcher

  def allTckScenarios: Seq[Scenario] = {
    // TODO: parse all features and return scenarios
    Seq.empty
  }

  def parseScenarios(path: String): Seq[Scenario] =
    parseFeatures(path).flatMap(_.scenarios)

  def parseFeatures(path: String): Seq[Feature] = {
    val directory = new File(path)
    require(directory.isDirectory)
    val features = directory.listFiles.filter(_.getName.endsWith(".feature"))
    features.map(parseFeature)
  }

  def parseFeature(f: File): Feature = {
    val featureString = Source
      .fromFile(f)
      .getLines
      .mkString("\n")
    val gherkinDocument = parser.parse(featureString, matcher)
    val compiler = new Compiler
    val pickles = compiler.compile(gherkinDocument).asScala
    val featureName = gherkinDocument.getFeature.getName
    val scenarios = pickles.map(toScenario(featureName, _))
    Feature(scenarios)
  }

  def toScenario(featureName: String, pickle: Pickle): Scenario = {
    val steps = pickle.getSteps.asScala.flatMap { step =>
      def arg = step.getArgument.asScala

      val scenarioStep = step.getText match {
        case "an empty graph" => None
        case "executing query:" =>
          require(arg.size == 1)
          Some(Execute(arg.head.asInstanceOf[PickleString].getContent))
        case "the result should be:" =>
          require(step.getArgument.size == 1)
          val rows = arg.head.asInstanceOf[PickleTable].getRows.asScala
          val header = rows.head
          val values = rows.tail
          val expected = values.map { v =>
            v.getCells.asScala
              .zip(header.getCells.asScala)
              .map {
                case (value, header) =>
                  header.getValue -> value.getValue
              }
              .toMap
          }.toList
          Some(Expect(expected))
        case "no side effects" =>
          Some(NoSideEffects)
      }
      scenarioStep
    }.toList
    Scenario(featureName, pickle.getName, steps)
  }

}
