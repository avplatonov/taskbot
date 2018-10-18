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
import ru.amontag.taskbot.rules.dict.NaiveContainsParser
import ru.amontag.taskbot.rules.dict.mystem.MystemRuleParser

import scala.collection.mutable

object CommonParser extends Parser {
    override val name: String = "common"

    private val actionParsers: Map[String, Parser] = List(
        MystemRuleParser,
        NaiveContainsParser,
        OrParser,
        AndParser
    ).map(p => p.name -> p).toMap

    override def parse(body: String): Either[Throwable, Rule] = {
        splitOntoActionNameAndBody(removeBraces(removeBreaks(body))) match {
            case Right(((actionName, threshold), actionBody)) =>
                actionParsers.get(actionName)
                    .map(p => p.parse(actionBody))
                    .map(p => p.map(_.withThreshold(threshold)))
                    .getOrElse(Left(new IllegalArgumentException(s"Cannot find action for body '$body'")))
            case Left(e) => Left(e)
        }
    }

    def main(args: Array[String]): Unit = {
        println(CommonParser.parse(
            "(or:0.5 \n\t(naive-contains \n\t\t(field header) \n\t\t(words мама мыла раму холодной тряпкой)) \n\t(and \n\t\t(morph-contains \n\t\t\t(field description) \n\t\t\t(words мама мыла раму холодной тряпкой)) \n\t\t(naive-contains \n\t\t\t(field header) \n\t\t\t(words aa, vv, 11))))"
        ))
    }

    private def prepareBody(body: String): String = removeBraces(removeBreaks(body))

    private def removeBreaks(body: String) = body.replaceAll("\\s+", " ")

    private def splitOntoActionNameAndBody(tok: String): Either[Throwable, ((String, Double), String)] = {
        splitBySpaces(tok) match {
            case Right(actionNameWithThreshold :: _) =>
                Right((getActionName(actionNameWithThreshold), tok.substring(actionNameWithThreshold.length).trim))
            case Left(e) => Left(e)
        }


    }

    private def getActionName(tok: String): (String, Double) = {
        tok.split(":").toList match {
            case n :: threshold :: Nil => (n.trim, threshold.toDouble)
            case n :: Nil => (n.trim, 1.0)
            case _ => throw new IllegalArgumentException()
        }
    }
}

trait Parser {
    val name: String

    def isCorrespondToRule(nameToken: String): Boolean = nameToken.equals(name)

    def parse(body: String): Either[Throwable, Rule]

    protected def removeBraces(tok: String): String = {
        assert(tok.startsWith("(") && tok.endsWith(")"), tok)
        tok.substring(1, tok.length - 1).trim
    }

    protected def splitBySpaces(body: String): Either[Throwable, List[String]] = {
        var cntOfOpenedBraces = 0
        var result = mutable.Buffer[String]()
        var currentToken = new mutable.StringBuilder()
        for (ch <- body.trim) {
            if(cntOfOpenedBraces < 0)
                return Left(new IllegalArgumentException(s"Invalid body format [$body]"))

            ch match {
                case ' ' if cntOfOpenedBraces == 0 =>
                    result += currentToken.toString()
                    currentToken.clear()
                case _ => {
                    ch match {
                        case '(' => cntOfOpenedBraces = cntOfOpenedBraces + 1
                        case ')' => cntOfOpenedBraces = cntOfOpenedBraces - 1
                        case _ =>
                    }

                    currentToken += ch
                }
            }
        }

        result += currentToken.toString()
        cntOfOpenedBraces match {
            case 0 => Right(result.toList)
            case _ => Left(new IllegalArgumentException(s"Invalid body format [$body]"))
        }
    }
}

trait SetOfRulesParser extends Parser {
    def parse(body: String): Either[Throwable, Rule] = {
        splitBySpaces(body)
            .map(_.map(_.trim))
            .map(_.map(CommonParser.parse)) match {

            case l@Left(e) => Left(e)
            case Right(rules) =>
                if (rules.isEmpty)
                    return Left(new IllegalArgumentException(s"There is no body in string '$body'"))

                rules.find(_.isLeft) match {
                    case Some(left) => return left
                    case _ => buildRule(rules.map({
                        case Right(rule) => rule
                    }))
                }
        }
    }

    protected def buildRule(rules: Seq[Rule]): Either[Throwable, Rule]
}

case class RuleTrace(ruleName: String, score: Double) {
    override def toString: String = s"[$ruleName:$score]"
}

trait Rule {
    val name: String

    val threshold: Double

    def predict(task: Task): Double

    def apply(task: Task): Boolean = predict(task) >= threshold

    def trace(task: Task): List[_]

    def withThreshold(value: Double): Rule
}

object TaskFieldParser {
    val tokenName: String = "field"

    def apply(token: String): Either[Throwable, Task => String] = token.toLowerCase match {
        case "header" => Right(_.header)
        case "description" => Right(_.description)
        case "files" => Right(_.files.mkString(","))
        case _ => Left(new IllegalArgumentException(s"Cannot find field with name '$token'"))
    }
}
