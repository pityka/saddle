package org.saddle.locatorall

import org.saddle.ST

private[saddle] trait LocatorAll[@scala.specialized(Boolean, Int, Long, Double) T] {

  /** Whether the instance contains the key
    * @param key
    *   The key to query
    */
  def contains(key: T): Boolean

  /** Should return all offset corresponding to the provided key, or empty Array if
    * none was found.
    * @param key
    *   The key to query
    */
  def get(key: T): Array[Int]

  def put(key: T, offset: Int) : Unit

}


private[saddle] object  LocatorAll {
   private[saddle] val empty = Array.empty[Int]

   def apply[@scala.specialized(Boolean, Int, Long, Double) C](sz: Int = 16)(implicit
      st: ST[C]
  ): LocatorAll[C] = st.makeLocAll(sz)

}