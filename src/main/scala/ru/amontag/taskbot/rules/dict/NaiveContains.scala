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

package ru.amontag.taskbot.rules.dict

import ru.amontag.taskbot.classifier.Task
import ru.amontag.taskbot.rules.{Rule, RuleTrace}

object NaiveContainsParser extends OnDictRuleParser {
    override val name: String = "naive-contains"

    override def buildRule(words: Set[String], taskFieldExtr: Task => String): Rule = NaiveContains(words, taskFieldExtr)

    override def ruleName: String = "naive-contains"
}

case class NaiveContains(words: Set[String], fieldExtractor: Task => String, threshold: Double = 1.0) extends Rule {
    override val name: String = "naive_contains"

    override def predict(task: Task): Double = {
        val text = fieldExtractor(task)
        val result = words.count(w => text.contains(w)).toDouble / words.size
        result
    }

    override def withThreshold(value: Double): Rule = copy(threshold = value)

    override def trace(task: Task): List[_] = List(RuleTrace(name, predict(task)))
}
