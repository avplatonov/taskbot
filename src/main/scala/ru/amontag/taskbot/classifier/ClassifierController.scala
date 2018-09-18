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

import java.{util => ju}

import org.springframework.web.bind.annotation._

import scala.collection.JavaConverters._

@RestController
class ClassifierController {
    @RequestMapping(
        value = Array("/classify"),
        method = Array(RequestMethod.GET)
    )
    @ResponseBody
    def get(@RequestBody obj: ju.Map[String, AnyRef]): ju.Map[String, AnyRef] = {
        Map[String, AnyRef]("parsing_result" -> parse(obj).toString).asJava
    }

    private def parse(obj: ju.Map[String, AnyRef]): Task = {
        val header = getOrThrow(obj, "header")
        val description = getOrThrow(obj, "description")
        val files = getListOfString(obj, "files")

        Task(
            header = header,
            description = description,
            files = files
        )
    }

    private def getOrThrow(obj: ju.Map[String, AnyRef], name: String): String = {
        val value = obj.get(name)
        checkNull(name, value)
        if (!value.isInstanceOf[String])
            throw new IllegalArgumentException(s"Cannot parse input object: field $name is not a String")

        value.asInstanceOf[String]
    }

    private def checkNull(name: String, value: AnyRef): Unit =
        if (value == null)
            throw new IllegalArgumentException(s"Cannot parse input object: field $name is not found")

    private def getListOfString(map: ju.Map[String, AnyRef], fieldName: String): List[String] = {
        val filesList = map.get(fieldName)
        checkNull(fieldName, filesList)

        filesList match {
            case l: ju.List[_] if l.isEmpty => Nil
            case l: ju.List[String] => l.asScala.toList
            case _ => throw new IllegalArgumentException(s"Cannot parse input object: field $fieldName is not a List")
        }
    }
}
