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
 */
package org.opencypher.okapi.impl.spark.io.hdfs

import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser.parse
import org.apache.spark.sql.types._

abstract class CsvSchema {
  def idField: CsvField
  def propertyFields: List[CsvField]

  def toStructType: StructType
}

// TODO: test
object CsvSchemaUtils {
  def parseJson[T](jsonString: String)(implicit decoder: Decoder[T]): T = {
    parse(jsonString) match {
      case Left(failure) => throw new RuntimeException(s"Invalid json file: $failure")
      case Right(json) =>
        json.hcursor.as[T] match {
          case Left(failure) => {
            val msg =
              s"Invalid JSON schema: Could not find mandatory element '${failure.history.head.productElement(0)}'"
            throw new RuntimeException(msg)
          }
          case Right(elem) => elem
        }
    }
  }
}

case class CsvField(name: String, column: Int, valueType: String) {
  private val listType = raw"list\[(\w+)\]".r

  /**
    * As CSV does not support list types they are represented as Strings and have to be read as such.
    * @return the Spark SQL type of the csv field at load time
    */
  lazy val getSourceType: DataType = valueType.toLowerCase match {
    case l if listType.pattern.matcher(l).matches() => StringType
    case other => extractSimpleType(other)
  }

  /**
    * For List types we return the target array type.
    * @return the Spark SQL type of the csv field at after special conversions
    */
  lazy val getTargetType: DataType = valueType.toLowerCase match {
    case l if listType.pattern.matcher(l).matches() => l match {
      case listType(inner) => ArrayType(extractSimpleType(inner))
    }

    case other => extractSimpleType(other)
  }

  lazy val toSourceStructField: StructField = StructField(name, getSourceType, nullable = true)

  lazy val toTargetStructField: StructField = StructField(name, getTargetType, nullable = true)

  private def extractSimpleType(typeString: String): DataType = typeString match {
    case "string"                  => StringType
    case "integer"                 => LongType
    case "long"                    => LongType
    case "boolean"                 => BooleanType
    case "float"                   => DoubleType
    case "double"                  => DoubleType
    case x                         => throw new RuntimeException(s"Unknown type $x")
  }
}

case class CsvNodeSchema(
    idField: CsvField,
    implicitLabels: List[String],
    optionalLabels: List[CsvField],
    propertyFields: List[CsvField])
    extends CsvSchema {

  def toStructType: StructType = {
    StructType(
      (List(idField) ++ optionalLabels ++ propertyFields)
        .sortBy(_.column)
        .map(_.toSourceStructField)
    )
  }
}

/**
  * Reads the schema of a node csv file. The schema file is in JSON format and has the following structure:
  * {
  *   "idField": {
  *     "name": "id",
  *     "column": 0,
  *     "valueType": "LONG"
  *   },
  *   "implicitLabels": ["Person","Employee"],
  *   "optionalLabels": [
  *     {
  *       "name": "Swede",
  *       "column": 3,
  *       "valueType": "BOOLEAN"
  *     },
  *     {
  *       "name": "German",
  *       "column": 4,
  *       "valueType": "BOOLEAN"
  *     }
  *   ],
  *   "propertyFields": [
  *     {
  *       "name": "name",
  *       "column": 1,
  *       "valueType": "STRING"
  *     },
  *     {
  *       "name": "luckyNumber",
  *       "column": 2,
  *       "valueType": "INTEGER"
  *     }
  *   ]
  * }
  */
object CsvNodeSchema {
  implicit val decodeNodeCsvSchema: Decoder[CsvNodeSchema] = for {
    idField <- Decoder.instance(_.get[CsvField]("idField"))
    implicitLabels <- Decoder.instance(_.get[List[String]]("implicitLabels"))
    optionalLabels <- Decoder.instance(_.getOrElse[List[CsvField]]("optionalLabels")(List()))
    propertyFields <- Decoder.instance(_.getOrElse[List[CsvField]]("propertyFields")(List()))
  } yield new CsvNodeSchema(idField, implicitLabels, optionalLabels, propertyFields)

  def apply(schemaJson: String): CsvNodeSchema = {
    CsvSchemaUtils.parseJson(schemaJson)
  }
}

case class CsvRelSchema(
    idField: CsvField,
    startIdField: CsvField,
    endIdField: CsvField,
    relType: String,
    propertyFields: List[CsvField])
    extends CsvSchema {

  def toStructType: StructType = {
    StructType(
      (List(idField, startIdField, endIdField) ++ propertyFields)
        .sortBy(_.column)
        .map(_.toSourceStructField)
    )
  }
}

/**
  * Reads the schema of a relationship csv file. The schema file is in JSON format and has the following structure:
  * {
  *   "idField": {
  *     "name": "id",
  *     "column": 0,
  *     "valueType": "LONG"
  *   },
  *   "startIdField": {
  *     "name": "start",
  *     "column": 1,
  *     "valueType": "LONG"
  *   },
  *   "endIdField": {
  *     "name": "end",
  *     "column": 2,
  *     "valueType": "LONG"
  *   },
  *   "relationshipType": "KNOWS",
  *   "propertyFields": [
  *     {
  *       "name": "since",
  *       "column": 3,
  *       "valueType": "INTEGER"
  *     }
  *   ]
  * }
  */
object CsvRelSchema {
  implicit val decodeRelCsvSchema: Decoder[CsvRelSchema] = for {
    id <- Decoder.instance(_.get[CsvField]("idField"))
    startIdField <- Decoder.instance(_.get[CsvField]("startIdField"))
    endIdField <- Decoder.instance(_.get[CsvField]("endIdField"))
    relType <- Decoder.instance(_.get[String]("relationshipType"))
    propertyFields <- Decoder.instance(_.getOrElse[List[CsvField]]("propertyFields")(List()))
  } yield new CsvRelSchema(id, startIdField, endIdField, relType, propertyFields)

  def apply(schemaJson: String): CsvRelSchema = {
    CsvSchemaUtils.parseJson(schemaJson)
  }
}