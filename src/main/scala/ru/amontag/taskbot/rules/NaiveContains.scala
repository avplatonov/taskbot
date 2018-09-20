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

object NaiveContainsParser extends Parser {
    override val name: String = "naive-contains"

    /**
      * rule text should correspond to this construction: (<rule_name>:threshold, (field <field-name>), (tokens t1,t2,...))
      */
    override def parse(body: String): Option[Rule] = {
        val tokens = removeBraces(body).split(",").map(_.toLowerCase.trim).toList
        tokens match {
            case nameWithTreshold :: fieldToken :: wordTokens :: Nil =>
                val (name, threshold) = getNameAndThreshold(nameWithTreshold)
                assert(name.equals(this.name))
                val fieldKeyword :: fieldName :: Nil = removeBraces(fieldToken).split(" ").toList
                val tokensKeyword :: words :: Nil = removeBraces(wordTokens).split(" ").toList
                assert(fieldKeyword.equals(TaskFieldParser.tokenName))
                assert(tokensKeyword.equals("tokens"))
                Some(NaiveContains(threshold, words, TaskFieldParser(fieldName)))
            case _ => None
        }
    }
}

case class NaiveContains(threshold: Double, str: String, fieldExtractor: Task => String) extends Rule {
    private val words: Set[String] = str.split(",").toSet

    override val name: String = "naive_contains"

    override def predict(task: Task): Double = {
        val text = fieldExtractor(task)
        words.count(w => text.contains(w)).toDouble / words.size
    }
}
