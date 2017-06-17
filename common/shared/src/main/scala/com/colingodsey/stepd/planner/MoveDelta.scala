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

import com.colingodsey.stepd.Math.Vector4D

object MoveDelta {
  val Empty = MoveDelta(Vector4D.Zero, Vector4D.Zero, 0)
}

final case class MoveDelta(from: Vector4D, to: Vector4D, f: Double) {
  val d = to - from
  val time = (d.length / f)
  val v = d / time
  val isValid = d.length > 0

  def length = d.length

  def isEOrZOnly = d.x == 0 && d.y == 0

  def scaleFr(scale: Double) = copy(f = f * scale)
}