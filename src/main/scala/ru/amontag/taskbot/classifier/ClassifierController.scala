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
    private var db: ScriptDB with ExportableScript = new ScriptDBOnString("(or:0.5 \n\t(naive-contains\n\t\t(field header) \n\t\t(words мама мыла раму холодной тряпкой)) \n\t(and \n\t\t(morph-contains:0.7\n\t\t\t(field description) \n\t\t\t(words мама мыла раму холодной тряпкой)) \n\t\t(naive-contains\n\t\t\t(field header) \n\t\t\t(words aa vv 11)))) -> stupid\n\n(morph-contains:0.7\n\t\t\t(field description) \n\t\t\t(words мама мыла раму холодной тряпкой)) -> stupid")

    @RequestMapping(
        value = Array("/classify"),
        method = Array(RequestMethod.GET)
    )
    @ResponseBody
    def get(@RequestBody obj: ju.Map[String, AnyRef]): String = {
        val task = parse(obj)
        db.get().apply(task).map({case (idx, ans) => s"$idx: $ans"}).mkString("\n")
    }

    @RequestMapping(
        value = Array("/script"),
        method = Array(RequestMethod.GET)
    )
    @ResponseBody
    def get(): String = db.export()

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
