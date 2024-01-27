/** Copyright (c) 2013 Saddle Development Team
  *
  * Licensed under the Apache License, Version 2.0 (the "License"); you may not
  * use this file except in compliance with the License. You may obtain a copy
  * of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  * License for the specific language governing permissions and limitations
  * under the License.
  */
package org.saddle.locatorall


import org.saddle.util.IntBufferMap

@scala.annotation.nowarn
private[saddle] class LocatorAllInt extends LocatorAll[Int] {
  val map = new IntBufferMap


  def contains(key: Int): Boolean = map.contains(key)
  def get(key: Int): Array[Int] = if (map.contains(key)) map.get(key).toArray else LocatorAll.empty
  def put(key: Int, value: Int) = 
    {map.update(key, value)}
   
  
}
