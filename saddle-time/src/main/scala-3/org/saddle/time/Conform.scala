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
package org.saddle.time

import scala.language.reflectiveCalls
import org.joda.time.DateTime
import scala.reflect.Selectable.reflectiveSelectable

private[saddle] trait Conform {

  /** Conforms a datetime to a recurrence rule either forward or backward.
    */
  def conform(rule: RRule, dt: DateTime, forward: Boolean = true): DateTime = {
    forward match {
      case true  => rule counting -1 from { rule counting +1 from dt }
      case false => rule counting +1 from { rule counting -1 from dt }
    }
  }
}
