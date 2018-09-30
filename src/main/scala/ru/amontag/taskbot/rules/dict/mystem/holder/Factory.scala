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

import java.io.File

import ru.amontag.taskbot.util.tool.mystem.external.FailSafeExternalProcessServer

import scala.util.Try

class Factory(parsingOptions: String = "-igd --eng-gr --format json --weight") {

    /**
      * Creates a new instance of mystem server
      * Uses .local if customExecutable was not set
      */
    def newMyStem(version: String, ex: File): Try[MyStem] = Try {
        version match {
            case "3.0" =>
                new MyStem30(
                    new FailSafeExternalProcessServer(
                        ex.getAbsolutePath + (if (parsingOptions.nonEmpty) " " + parsingOptions else "")))
            case _ => throw new NotImplementedError()
        }
    }
}
