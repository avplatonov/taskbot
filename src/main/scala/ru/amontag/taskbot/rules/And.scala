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
  * (and:threshold (rule) (rule) ... (rule))
  */
object AndParser extends Parser {
    override val name: String = "and"

    override def parse(body: String): Option[Rule] = {
        removeBraces(body).split(" ").map(_.toLowerCase().trim).toList match {
            case ruleNameWithTreshold :: rulesStrings if rulesStrings.nonEmpty =>
                getNameAndThreshold(ruleNameWithTreshold) match {
                    case ("and", threshold) =>
                        val rules = rulesStrings.map(Parser.parse)
                        assert(rules.forall(_.isDefined))
                        Some(And(threshold, rules.flatten))
                    case _ => None
                }
            case _ => None
        }
    }
}

case class And(threshold: Double, subrules: Seq[Rule]) extends Rule {
    override val name: String = "and"

    override def predict(task: Task): Double = subrules.map(_.predict(task)).product
}
