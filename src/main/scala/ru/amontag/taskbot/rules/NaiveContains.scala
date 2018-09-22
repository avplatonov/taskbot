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

    private val FIELD = "field"
    private val WORDS = "words"

    private val format = "(naive-contains[:threshold] (field <field-name>) (words <t1>[, <t2>]*))"

    override def parse(body: String): Either[Throwable, Rule] = {
        splitBySpaces(body).map(_.toList) match {
            case Right(fieldNameTok :: wordsToken :: Nil) =>
                val fieldNameTokSpltd = removeBraces(fieldNameTok).split(' ').toList
                val wordsTokSpltd = removeBraces(wordsToken).replaceAll(",\\s+", ",").split(' ').toList
                if(fieldNameTokSpltd.size != 2 || wordsTokSpltd.size != 2 ||
                    !fieldNameTokSpltd.head.equals(FIELD) || !wordsTokSpltd.head.equals(WORDS))

                    return invalidFormat(body)
                else {
                    val _ :: fieldName :: Nil = fieldNameTokSpltd
                    val _ :: words :: Nil = wordsTokSpltd
                    return TaskFieldParser(fieldName).map(NaiveContains(words.split(',').toSet, _))
                }

            case _ => invalidFormat(body)
        }
    }

    private def invalidFormat(body: String): Either[Throwable, Rule] =
        Left(new IllegalArgumentException(s"Action format is invalid: use $format [body: $body]"))
}

case class NaiveContains(words: Set[String], fieldExtractor: Task => String, threshold: Double = 1.0) extends Rule {
    override val name: String = "naive_contains"

    override def predict(task: Task): Double = {
        val text = fieldExtractor(task)
        words.count(w => text.contains(w)).toDouble / words.size
    }

    override def withThreshold(value: Double): Rule = copy(threshold = value)
}
