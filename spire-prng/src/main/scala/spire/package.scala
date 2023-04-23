package org.saddle
package object spire {
  private[spire] type sp = scala.specialized
  private[spire] type tailrec = scala.annotation.tailrec
  private[spire] type ClassTag[A] = scala.reflect.ClassTag[A]
  private[spire] val ClassTag = scala.reflect.ClassTag
}
