/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ru.amontag.taskbot.rules

import ru.amontag.taskbot.classifier.Task


object Parser extends Parser {
    override val name: String = "common"

    override def parse(body: String): Option[Rule] = ???
}

trait Parser {
    val name: String

    def isCorrespondToRule(nameToken: String): Boolean = nameToken.equals(name)

    def parse(body: String): Option[Rule]

    def removeBraces(tok: String): String = {
        assert(tok.startsWith("(") && tok.endsWith(")"))
        tok.substring(1, tok.length - 1).trim
    }

    def getNameAndThreshold(tok: String): (String, Double) = {
        tok.split(":").toList match {
            case n :: threshold :: Nil => (n, threshold.toDouble)
            case n => (n, 1.0)
            case _ => throw new IllegalArgumentException()
        }
    }
}

trait Rule {
    val name: String

    val threshold: Double

    def predict(task: Task): Double

    def apply(task: Task): Boolean = predict(task) >= threshold
}

object TaskFieldParser {
    val tokenName: String = "field"

    def apply(token: String): Task => String = token.toLowerCase match {
        case "header" => _.header
        case "description" => _.description
        case "files" => _.files.mkString(",")
        case _ => throw new IllegalArgumentException()
    }
}
