package org.saddle

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

package object jsoniter {

  implicit def indexCodec[T: JsonValueCodec: ST: ORD]
      : JsonValueCodec[Index[T]] =
    new JsonValueCodec[Index[T]] {
      locally {
        val _ = implicitly[JsonValueCodec[T]]
      }
      val codec0: JsonValueCodec[Seq[T]] = JsonCodecMaker.make
      override def decodeValue(in: JsonReader, default: Index[T]): Index[T] =
        Index(codec0.decodeValue(in, codec0.nullValue): _*)

      override def encodeValue(
          x: Index[T],
          out: JsonWriter
      ): _root_.scala.Unit = codec0.encodeValue(x.toSeq, out)

      override def nullValue: Index[T] = null.asInstanceOf[Index[T]]
    }

  implicit def vecCodec[T: JsonValueCodec: ST]: JsonValueCodec[Vec[T]] =
    new JsonValueCodec[Vec[T]] {
      locally {
        val _ = implicitly[JsonValueCodec[T]]
      }
      val codec0: JsonValueCodec[Seq[T]] = JsonCodecMaker.make
      override def decodeValue(in: JsonReader, default: Vec[T]): Vec[T] =
        Vec(codec0.decodeValue(in, codec0.nullValue): _*)

      override def encodeValue(
          x: Vec[T],
          out: JsonWriter
      ): _root_.scala.Unit = codec0.encodeValue(x.toSeq, out)

      override def nullValue: Vec[T] = null.asInstanceOf[Vec[T]]
    }

  implicit def matCodec[T: JsonValueCodec: ST]: JsonValueCodec[Mat[T]] =
    new JsonValueCodec[Mat[T]] {
      locally {
        val _ = implicitly[JsonValueCodec[T]]
      }
      val codec0: JsonValueCodec[(Int, Int, Vec[T])] = JsonCodecMaker.make
      override def decodeValue(in: JsonReader, default: Mat[T]): Mat[T] = {
        val (n, m, v) = codec0.decodeValue(in, codec0.nullValue)
        Mat(n, m, v)
      }

      override def encodeValue(
          x: Mat[T],
          out: JsonWriter
      ): _root_.scala.Unit =
        codec0.encodeValue((x.numRows, x.numCols, x.toVec), out)

      override def nullValue: Mat[T] = null.asInstanceOf[Mat[T]]
    }

  implicit def seriesCodec[T: JsonValueCodec: ST, I: JsonValueCodec: ST: ORD]
      : JsonValueCodec[Series[I, T]] =
    new JsonValueCodec[Series[I, T]] {
      locally {
        val _ = implicitly[JsonValueCodec[T]]
        val _ = implicitly[JsonValueCodec[I]]
      }
      val codec0: JsonValueCodec[(Index[I], Vec[T])] = JsonCodecMaker.make
      override def decodeValue(
          in: JsonReader,
          default: Series[I, T]
      ): Series[I, T] = {
        val (i, v) = codec0.decodeValue(in, codec0.nullValue)
        Series(i, v)
      }

      override def encodeValue(
          x: Series[I, T],
          out: JsonWriter
      ): _root_.scala.Unit =
        codec0.encodeValue((x.index, x.toVec), out)

      override def nullValue: Series[I, T] = null.asInstanceOf[Series[I, T]]
    }
  implicit def frameCodec[
      T: JsonValueCodec: ST,
      I: JsonValueCodec: ST: ORD,
      I2: JsonValueCodec: ST: ORD
  ]: JsonValueCodec[Frame[I, I2, T]] =
    new JsonValueCodec[Frame[I, I2, T]] {
      locally {
        val _ = implicitly[JsonValueCodec[T]]
        val _ = implicitly[JsonValueCodec[I]]
        val _ = implicitly[JsonValueCodec[I2]]
      }
      val codec0: JsonValueCodec[(Vec[I], Vec[I2], Vec[T])] =
        JsonCodecMaker.make
      override def decodeValue(
          in: JsonReader,
          default: Frame[I, I2, T]
      ): Frame[I, I2, T] = {
        val (i, i2, v) = codec0.decodeValue(in, codec0.nullValue)
        Frame.apply(Mat(i.length, i2.length, v), Index(i), Index(i2))
      }

      override def encodeValue(
          x: Frame[I, I2, T],
          out: JsonWriter
      ): _root_.scala.Unit =
        codec0.encodeValue((x.rowIx.toVec, x.colIx.toVec, x.toMat.toVec), out)

      override def nullValue: Frame[I, I2, T] =
        null.asInstanceOf[Frame[I, I2, T]]
    }

}
