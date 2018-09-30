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

package ru.amontag.taskbot.util.tool.mystem

import java.net.URL

import com.typesafe.config.ConfigFactory

object Properties {

    val BinDestination = System.getProperty("user.home") + "/.local/bin/"

    private val systemOsName = System.getProperty("os.name")
    private val systemOsArchitecture = System.getProperty("os.arch")
    val CurrentOs = os(systemOsName, systemOsArchitecture)

    val BIN_FILE_NAME = CurrentOs match {
        case name if name.startsWith("win") => "mystem.exe"
        case name => "mystem"
    }

    private lazy val rootProp = ConfigFactory.load("mystem-sources.conf")
    private lazy val version = rootProp.getConfig("version")

    private val versionPattern = "\\d+\\.\\d+".r.pattern

    private def doOrDie[T](action: => T, message: String = "Unknown error"): T =
        try action
        catch {
            case e: Throwable => throw new Exception(message)
        }

    @throws(classOf[Exception])
    def getUrl(versionRaw: String, os: String = CurrentOs): URL = {

        require(versionPattern.matcher(versionRaw).matches,
            "Troubles with version name, should match pattern <number>.<number>")

        val versionProps =
            doOrDie(
                version.getConfig(versionRaw),
                s"No binaries sources for version [$versionRaw] found")

        val url =
            doOrDie(
                versionProps.getString(os),
                s"Version number is correct, no binaries sources for OS [$os] found")

        doOrDie(
            new URL(url),
            s"URL configs troubles. If you see this message, please email anton.m.alexeyev@gmail.com")
    }
}
