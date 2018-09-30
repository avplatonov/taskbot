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

package ru.amontag.taskbot.rules.dict.mystem.parsing

import org.json.JSONArray
import ru.amontag.taskbot.rules.dict.mystem.model.Info

object JsonRepresentationParser {

    def toInfo(json: String): Traversable[Info] = toInfo(new JSONArray(json))

    private def toInfo(json: JSONArray): Traversable[Info] = {

        //todo: fix and enable GrammarInfo parsing

        val stuff: Traversable[Info] =
            for (i <- 0 until json.length)
                yield {
                    val item = json.getJSONObject(i)
                    val initial = item.getString("text")

                    if (item.has("analysis")) {
                        val analysis = item.getJSONArray("analysis")

                        if (analysis.length() == 0)
                            Info(initial, None, item.toString)
                        else {
                            val result =
                                for (j <- 0 until analysis.length)
                                    yield {
                                        val anItem = analysis.getJSONObject(j)
                                        new Info(initial, Option(anItem.getString("lex")), item.toString)
                                    }
                            result.head
                        }
                    } else {
                        Info(initial, None, item.toString)
                    }

                }
        stuff
    }
}
