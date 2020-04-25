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
  //Add a an extra line break or two incase a byte gets lost
  val chunkFooter = ByteString.fromString("\r\n") //ByteString.empty

  def apply(bytes: Array[Byte]): Chunk =
    Chunk(ByteString(bytes))

  def apply(bytes: Seq[Byte]): Chunk =
    Chunk(ByteString(bytes: _*))

  def getUnlockPageBytes(idx: Int) = {
    val idxByte = ByteString(idx.toByte)
    chunkHeader ++ idxByte ++ ByteString(0.toByte) ++ chunkFooter
  }
}

case class Chunk(rawBytes: ByteString) {
  import Chunk._

  require(rawBytes.length == StepProcessor.BytesPerChunk)

  def produceBytes(idx: Int, corrupt: Boolean = false): ByteString = {
    val testCorruption = if (corrupt) 1 else 0

    val check = (rawBytes.foldLeft(0)(_ ^ _) & 0xFF) + testCorruption
    val idxByte = ByteString(idx.toByte)

    chunkHeader ++ idxByte ++ rawBytes ++ ByteString(check.toByte) ++ chunkFooter
  }
}