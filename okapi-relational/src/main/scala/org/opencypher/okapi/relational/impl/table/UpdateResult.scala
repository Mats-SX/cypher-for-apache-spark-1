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
package org.opencypher.okapi.relational.impl.table

sealed trait UpdateResult[+T] {
  def it: T
}

// Non-Failed
sealed trait SuccessfulUpdateResult[+T] extends UpdateResult[T]

sealed trait FailedUpdateResult[+T] extends UpdateResult[T]

// Non-Removing
sealed trait AdditiveUpdateResult[+T] extends UpdateResult[T]

// Non-Additive
sealed trait RemovingUpdateResult[+T] extends UpdateResult[T]

// Neither Failed nor Found
sealed trait MutatingUpdateResult[+T] extends UpdateResult[T]

final case class Added[T](it: T)
  extends SuccessfulUpdateResult[T] with AdditiveUpdateResult[T] with MutatingUpdateResult[T]

final case class Replaced[T](old: T, it: T)
  extends SuccessfulUpdateResult[T] with AdditiveUpdateResult[T] with MutatingUpdateResult[T]

final case class Found[T](it: T)
  extends SuccessfulUpdateResult[T] with AdditiveUpdateResult[T]

final case class FailedToAdd[T](
  conflict: T,
  update: SuccessfulUpdateResult[T] with AdditiveUpdateResult[T] with MutatingUpdateResult[T]
) extends FailedUpdateResult[T] with AdditiveUpdateResult[T] {

  def it = update.it
}

final case class Removed[T](it: T, dependents: Set[T] = Set.empty)
  extends SuccessfulUpdateResult[T] with MutatingUpdateResult[T] with RemovingUpdateResult[T]

final case class NotFound[T](it: T)
  extends SuccessfulUpdateResult[T] with RemovingUpdateResult[T]

final case class FailedToRemove[T](
 conflict: T,
 update: SuccessfulUpdateResult[T] with RemovingUpdateResult[T] with MutatingUpdateResult[T]
) extends FailedUpdateResult[T] with RemovingUpdateResult[T] {

  def it: T = update.it
}
