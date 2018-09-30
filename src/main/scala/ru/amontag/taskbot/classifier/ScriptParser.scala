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

package ru.amontag.taskbot.classifier

import ru.amontag.taskbot.answer.AnswerTemplate
import ru.amontag.taskbot.rules.{CommonParser, Rule}

case class ParsingException(script: String) extends RuntimeException

object ScriptParser {
    private val regex = "\\(.*?\\)\\s+->\\s+\\w+".r

    def parseWithThresholds(script: String): Either[Throwable, List[(Rule, AnswerTemplate)]] = {
        val normalizedText = script.replaceAll("\\s+", " ")
        val parsingResult = regex.findAllIn(normalizedText).map({
            case ruleWithAnswer => ruleWithAnswer.split(" -> ").toList match {
                case rule :: answer :: Nil =>
                    CommonParser.parse(rule).flatMap(r => AnswerTemplate.apply(answer) match {
                        case Left(e) => Left(e)
                        case Right(template) => Right(r -> template)
                    })
                case _ => throw ParsingException(script)
            }
        }).toList

        parsingResult.find(_.isLeft) match {
            case None => Right(parsingResult.map(_.getOrElse(throw new IllegalStateException())))
            case Some(Left(e)) => Left(e)
        }
    }

    def main(args: Array[String]): Unit = {
        println(parseWithThresholds("(or:0.5 \n\t(naive-contains \n\t\t(field header) \n\t\t(words мама мыла раму холодной тряпкой)) \n\t(and \n\t\t(morph-contains \n\t\t\t(field description) \n\t\t\t(words мама мыла раму холодной тряпкой)) \n\t\t(naive-contains \n\t\t\t(field header) \n\t\t\t(words aa, vv, 11)))) -> stupid\n\n(morph-contains \n\t\t\t(field description) \n\t\t\t(words мама мыла раму холодной тряпкой)) -> stupid"))
    }
}
