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

/**
  * (or:threshold (rule) (rule) ... (rule))
  */
object OrParser extends Parser {
    override val name: String = "or"

    override def parse(body: String): Option[Rule] = {
        removeBraces(body).split(" ").map(_.toLowerCase().trim).toList match {
            case ruleNameWithTreshold :: rulesStrings if rulesStrings.nonEmpty =>
                getNameAndThreshold(ruleNameWithTreshold) match {
                    case ("or", threshold) =>
                        val rules = rulesStrings.map(Parser.parse)
                        assert(rules.forall(_.isDefined))
                        Some(Or(threshold, rules.flatten))
                    case _ => None
                }
            case _ => None
        }
    }
}

case class Or(threshold: Double, subrules: Seq[Rule]) extends Rule {
    override val name: String = "or"

    override def predict(task: Task): Double = subrules.map(_.predict(task)).sum
}
