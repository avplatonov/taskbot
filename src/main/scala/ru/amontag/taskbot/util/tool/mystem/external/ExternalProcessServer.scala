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

package ru.amontag.taskbot.util.tool.mystem.external

import java.io.{BufferedReader, BufferedWriter, OutputStreamWriter}
import java.nio.charset.Charset

import scala.io.Source
import scala.util._

private[external] class ExternalProcessServer(starterCommand: String) extends SyncServer {

    private val p = Runtime.getRuntime.exec(starterCommand)
    private val (in, out, err) = (p.getInputStream, p.getOutputStream, p.getErrorStream)

    private val writer = new BufferedWriter(new OutputStreamWriter(out, Charset.forName("utf-8")), 1)
    private val reader = Source.fromInputStream(in).reader()
    private val bufferedReader = new BufferedReader(reader)

    def syncRequest(request: String): Try[String] = Try {

        writer.write(request)
        writer.newLine()
        writer.flush()

        while (!bufferedReader.ready()) {}

        val builder = new StringBuilder()
        while (bufferedReader.ready) builder.append(bufferedReader.readLine())
        builder.toString()
    }

    def isAlive: Boolean = {
        Try(p.exitValue()) match {
            case Success(_) => false
            case Failure(e: IllegalThreadStateException) => true
            case Failure(e) => throw new RuntimeException(e) // unknown exception
        }
    }

    def kill() {
        p.destroy()
    }
}
