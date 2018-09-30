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

import ru.amontag.taskbot.rules.dict.mystem.model._

object GrammarInfoParsing {

    /**
      * Grammar info parsing.
      */
    def toGrammarInfo(commaSeparatedTags: String): GrammarInfo = {

        val mappedEnums =
            (commaSeparatedTags
                .split("[,=]")
                .map {
                    case name: String =>
                        val obj: Enumeration = GrammarMapBuilder.tagToEnumMap(name)
                        (obj, obj.withName(name))
                } groupBy {
                case (obj: Enumeration, _) => obj
            } mapValues {
                case array => array.map(_._2)
            }).toMap

        def findByEnum[T <: scala.Enumeration](enum: T): Set[T#Value] =
            mappedEnums
                .get(enum)
                .map(_.map(_.asInstanceOf[T#Value]).toSet)
                .getOrElse(Set.empty[T#Value])

        GrammarInfo(
            pos = findByEnum(POS),
            tense = findByEnum(Tense),
            `case` = findByEnum(Case),
            number = findByEnum(Number),
            verbFormInfo = findByEnum(VerbForms),
            adjFormInfo = findByEnum(AdjectiveForms),
            gender = findByEnum(Gender),
            aspect = findByEnum(Aspect),
            voice = findByEnum(Voice),
            animacy = findByEnum(Animacy),
            other = findByEnum(Other)
        )
    }

    def toStringRepresentation(gi: GrammarInfo): String =
        (gi.`case` ++ gi.adjFormInfo ++ gi.animacy ++ gi.aspect ++ gi.gender ++
            gi.number ++ gi.pos ++ gi.other ++ gi.tense ++ gi.verbFormInfo ++ gi.voice).mkString(",")
}
