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

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import ru.amontag.taskbot.answer.AnswerTemplate
import ru.amontag.taskbot.rules.Rule

import scala.io.Source

case class Script(rules: List[(Rule, AnswerTemplate)]) {
    def trace(task: Task): List[(_, String)] = {
        rules map {
            case (rule, template) =>
                rule.trace(task) -> template.buildAnswer(task)
        }
    }

    def apply(task: Task): List[(Int, String)] = rules.zipWithIndex filter {
        case ((rule, _), _) => rule.apply(task)
    } map {
        case ((_, template), idx) => idx -> template.buildAnswer(task)
    }
}

trait ScriptDB {
    def get(): Script

    def save(script: Script): Unit
}

trait ExportableScript {
    def export(): String
}

trait InMemoryScriptDB extends ScriptDB with ExportableScript {
    protected val scriptRef: AtomicReference[Script]

    override def get(): Script = scriptRef.get()

    override def save(script: Script): Unit = scriptRef.set(script)
}

class ScriptDBOnString(scriptStr: String) extends InMemoryScriptDB {
    override protected val scriptRef = new AtomicReference[Script](
        ScriptParser.parseWithThresholds(scriptStr) match {
            case Left(e) => throw e
            case Right(s) => Script(s)
        }
    )

    override def export(): String = scriptStr
}

class ScriptDBFromFile(path: Path) extends InMemoryScriptDB {
    private val db = new ScriptDBOnString(Source.fromFile(path.toFile).getLines()
        .mkString(" "))

    override protected val scriptRef = new AtomicReference[Script](db.get())

    override def export(): String = db.export()
}
