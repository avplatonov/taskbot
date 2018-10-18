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
  * (or[:threshold] (rule) (rule) ... (rule))
  */
object OrParser extends SetOfRulesParser {
    override val name: String = "or"

    override protected def buildRule(rules: Seq[Rule]): Either[Throwable, Rule] = Right(Or(rules))
}

case class Or(subrules: Seq[Rule], threshold: Double = 1.0) extends Rule {
    override val name: String = "or"

    override def predict(task: Task): Double = {
        val votes = subrules.map(_.predict(task))
        votes.max
    }

    override def withThreshold(value: Double): Rule = copy(threshold = value)

    override def trace(task: Task): List[_] = List(RuleTrace(name, predict(task)), subrules.map(_.trace(task)).toList)
}
