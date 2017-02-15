package org.opencypher.spark.impl.prototype

import java.lang.reflect.{InvocationHandler, Method}

import org.neo4j.cypher.internal.compiler.v3_2.AstRewritingMonitor
import org.neo4j.cypher.internal.compiler.v3_2.phases.{CompilationPhases, CompilationState}
import org.neo4j.cypher.internal.frontend.v3_2.ast.Statement
import org.neo4j.cypher.internal.frontend.v3_2.helpers.rewriting.RewriterStepSequencer
import org.neo4j.cypher.internal.frontend.v3_2.phases._
import org.neo4j.cypher.internal.frontend.v3_2.{CypherException, InputPosition}
import org.opencypher.spark.api.{CypherResultContainer, PropertyGraph}

import scala.reflect.ClassTag

trait Prototype {
  def cypher(query: String): CypherResultContainer = {
    val startState = CompilationState(query, None, null)
    val endState = pipeLine.transform(startState, FrontendContext())

    //    val semanticState = endState.semantics
    val params = endState.extractedParams
    val rewritten = endState.statement

    val ir = SparkQueryGraph.from(rewritten)
    val plan = planner.plan(ir)

    val result = graph.cypher(plan)

    result
  }

  def graph: PropertyGraph

  val planner = new SupportedQueryPlanner

  val pipeLine =
    CompilationPhases.parsing(RewriterStepSequencer.newPlain) andThen
      CompilationPhases.lateAstRewriting
}

//trait CypherResult[T] {
//  def get(): T
//}

case class FrontendContext() extends BaseContext {
  override def tracer: CompilationPhaseTracer = CompilationPhaseTracer.NO_TRACING

  override def notificationLogger: InternalNotificationLogger = devNullLogger

  override def exceptionCreator: (String, InputPosition) => CypherException = (_, _) => null

  override def monitors: Monitors = new Monitors {
    override def newMonitor[T <: AnyRef : ClassTag](tags: String*): T = {
      new AstRewritingMonitor {
        override def abortedRewriting(obj: AnyRef): Unit = ???
        override def abortedRewritingDueToLargeDNF(obj: AnyRef): Unit = ???
      }
    }.asInstanceOf[T]

    override def addMonitorListener[T](monitor: T, tags: String*) = ???
  }
}
