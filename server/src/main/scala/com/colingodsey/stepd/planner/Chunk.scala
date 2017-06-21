/*
 * Copyright 2017 Colin Godsey
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.colingodsey.stepd.planner

import akka.util.ByteString

object Chunk {
  val chunkHeader = ByteString.fromString("!")
  val chunkFooter = ByteString.empty //ByteString.fromString("\r\n")

  def apply(bytes: Array[Byte]): Chunk =
    Chunk(ByteString(bytes))

  def apply(bytes: Seq[Byte]): Chunk =
    Chunk(ByteString(bytes: _*))
}

case class Chunk(rawBytes: ByteString) {
  import Chunk._

  require(rawBytes.length == StepProcessor.BytesPerChunk)

  val check = rawBytes.foldLeft(0)(_ ^ _) & 0xFF
  val chunk = chunkHeader ++ rawBytes ++ ByteString(check.toByte) ++ chunkFooter

  def length = chunk.length
}