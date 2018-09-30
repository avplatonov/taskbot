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

package ru.amontag.taskbot.rules.dict.mystem.holder

import ru.amontag.taskbot.rules.dict.mystem.model.Info
import ru.amontag.taskbot.rules.dict.mystem.parsing.JsonRepresentationParser
import ru.amontag.taskbot.util.tool.mystem.external.FailSafeExternalProcessServer

import scala.util.{Failure, Success}

case class Request(text: String)

case class Response(info: Traversable[Info])

trait MyStem {

    def normalize(text: String) = text.replaceAll("\n", " ")

    @throws(classOf[MyStemApplicationException])
    def analyze(request: Request): Response
}

class MyStemApplicationException(e: Throwable) extends java.lang.Exception

// We need this because mystem.v < 3.0 doesn't support json AFAIK
class MyStem30(s: FailSafeExternalProcessServer) extends MyStem {

    @throws(classOf[MyStemApplicationException])
    override def analyze(request: Request): Response = {
        s.syncRequest(normalize(request.text)) match {
            case Failure(e) => throw new MyStemApplicationException(e)
            case Success(json) => Response(JsonRepresentationParser.toInfo(json))
        }
    }
}
