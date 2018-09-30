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

package ru.amontag.taskbot.answer

import ru.amontag.taskbot.classifier.Task

case class TemplateNotFoundException(name: String) extends RuntimeException

object AnswerTemplate {
    private val templates: Map[String, AnswerTemplate] = List[AnswerTemplate](
        StupidAnswer("ваша заявка очень важна для нас, мы приняли её в работу")
    ).map(x => x.name -> x).toMap

    def apply(name: String): Either[Exception, AnswerTemplate] = {
        templates.get(name) match {
            case Some(template) => Right(template)
            case None => Left(TemplateNotFoundException(name))
        }
    }
}

trait AnswerTemplate {
    def name: String

    def buildAnswer(task: Task): String
}

case class StupidAnswer(text: String) extends AnswerTemplate {
    override def name: String = "stupid"

    override def buildAnswer(task: Task): String = text
}
