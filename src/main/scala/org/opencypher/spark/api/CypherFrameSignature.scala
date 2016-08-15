package org.opencypher.spark.api

import org.opencypher.spark.impl.PlanningContext

trait CypherFrameSignature {

  type Field <: CypherField
  type Slot <: CypherSlot

  def field(sym: Symbol): Field
  def slot(field: Field): Slot
  def slot(name: Symbol): Slot

  def addField(pair: (Symbol, CypherType))(implicit context: PlanningContext): (Field, CypherFrameSignature)
  def aliasField(oldField: Symbol, newField: Symbol): (Field, CypherFrameSignature)
  def selectFields(fields: Field*): (CypherFrameSignature, Seq[Slot])

  def slots: Seq[Slot]
  def fields: Seq[Field]
}