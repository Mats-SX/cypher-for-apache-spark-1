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
package org.opencypher.spark.examples

import java.net.URI

import org.neo4j.driver.v1.Session
import org.neo4j.graphdb.Result
import org.neo4j.harness.{ServerControls, TestServerBuilders}
import org.opencypher.spark.api.io.neo4j.Neo4jConfig

object Neo4jHelpers {

  implicit class RichServerControls(val server: ServerControls) extends AnyVal {

    def dataSourceConfig =
      Neo4jConfig(server.boltURI(), user = "anonymous", password = Some("password"), encrypted = false)

    def uri: String = {
      val scheme = server.boltURI().getScheme
      val userInfo = s"anonymous:password@"
      val host = server.boltURI().getAuthority
      s"$scheme://$userInfo$host"
    }

    def stop(): Unit = {
      server.close()
    }

    def execute(cypher: String): Result =
      server.graph().execute(cypher)
  }

  def startNeo4j(dataFixture: String): ServerControls = {
    TestServerBuilders
      .newInProcessBuilder()
      .withConfig("dbms.security.auth_enabled", "true")
      .withFixture("CALL dbms.security.createUser('anonymous', 'password', false)")
      .withFixture(dataFixture)
      .newServer()
  }

  def connectLocally(
    fixture: String,
    user: String,
    pw: String,
    host: String = "bolt://localhost",
    port: Int = 7687
  ): Neo4jSession = {
    val config = Neo4jConfig(URI.create(s"$host:$port"), user = user, password = Some(pw))
    val session = config.driver().session()
    session.run(fixture)
    Neo4jSession(session, config)
  }

  case class Neo4jSession(session: Session, config: Neo4jConfig) {
    def close(): Unit = {
      session.run("MATCH (n) DETACH DELETE n")
      session.close()
    }
  }
}
