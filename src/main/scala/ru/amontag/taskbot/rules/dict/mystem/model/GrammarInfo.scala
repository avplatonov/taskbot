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

package ru.amontag.taskbot.rules.dict.mystem.model

case class GrammarInfo(pos: Set[POS.Value] = Set.empty,
    tense: Set[Tense.Value] = Set.empty,
    `case`: Set[Case.Value] = Set.empty,
    number: Set[Number.Value] = Set.empty,
    verbFormInfo: Set[VerbForms.Value] = Set.empty[VerbForms.Value],
    adjFormInfo: Set[AdjectiveForms.Value] = Set.empty[AdjectiveForms.Value],
    gender: Set[Gender.Value] = Set.empty,
    aspect: Set[Aspect.Value] = Set.empty,
    voice: Set[Voice.Value] = Set.empty,
    animacy: Set[Animacy.Value] = Set.empty,
    other: Set[Other.Value] = Set.empty[Other.Value])
