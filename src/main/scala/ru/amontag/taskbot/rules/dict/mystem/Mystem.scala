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

package ru.amontag.taskbot.rules.dict.mystem

import java.io.File

import ru.amontag.taskbot.classifier.Task
import ru.amontag.taskbot.rules.Rule
import ru.amontag.taskbot.rules.dict.OnDictRuleParser
import ru.amontag.taskbot.rules.dict.mystem.holder.Request


object Mystem {
    private val analyzer = new holder.Factory("-igd --eng-gr --format json --weight")
        .newMyStem("3.0", new File(getClass.getResource("/libs/mystem").toURI))
        .get

    def normalize(text: String): Traversable[String] = analyzer.analyze(Request(text)).info.flatMap(x => x.lex)
}

object MystemRuleParser extends OnDictRuleParser {
    override val name: String = "morph-contains"

    override def buildRule(words: Set[String], taskFieldExtr: Task => String): Rule = MystemRule(words, taskFieldExtr)

    override def ruleName: String = "morph-contains"
}

case class MystemRule(words: Set[String], fieldExtractor: Task => String, threshold: Double = 1.0) extends Rule {
    private val normalizedDict: Set[String] = words.flatMap(Mystem.normalize).toSet

    override val name: String = MystemRuleParser.name

    override def predict(task: Task): Double = {
        val normalizedWords = Mystem.normalize(fieldExtractor(task)).toSet
        val result = normalizedWords.count(normalizedDict).toDouble / normalizedDict.size
        result
    }

    override def withThreshold(value: Double): Rule = this.copy(threshold = value)

    override def toString: String = s"MystemRule($normalizedDict, $threshold)"
}
