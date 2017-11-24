package org.opencypher.caps.test.support

import org.apache.spark.sql.Row

import scala.collection.Bag
import scala.collection.immutable.HashedBagConfiguration

trait DebugOutputSupport {
  implicit class RowPrinter(bag: Bag[Row]) {
    def debug(): String = {
      val rowStrings = bag.map { row =>
        val rowAsString = row.toSeq.map {
          case null => "null"
          case s: String => s""""$s""""
          case l: Long => s"""${l}L"""
          case other => other.toString
        }

        rowAsString.mkString("Row(", ", ", ")")
      }

      rowStrings.mkString("Bag(", ",\n", ")")
    }
  }

  implicit class IterableToBagConverter(val elements: Iterable[Row]) {
    def toBag: Bag[Row] = Bag(elements.toSeq: _*)
  }

  implicit class ArrayToBagConverter(val elements: Array[Row]) {
    def toBag: Bag[Row] = Bag(elements.toSeq: _*)
  }

  // needed for bag builder initialization
  implicit val m: HashedBagConfiguration[Row] = Bag.configuration.compact[Row]
}