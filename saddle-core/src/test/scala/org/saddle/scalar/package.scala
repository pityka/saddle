package org.saddle
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

package object scalar {

  /** Existential types for type constructors. Turns type constructors into
    * parameter-less types usable in collections or scalacheck generators.
    */
  trait E[F[_]] {
    type T
    val value: F[T]
    override def toString(): String = s"E($value)"
  }

  object E {
    def apply[F[_], T1](value1: F[T1]) = new E[F] {
      type T = T1
      val value = value1
    }
  }

  trait ScalarGen[T] {
    val tag: ScalarTag[T]
    val gen: Gen[T]
    override def toString(): String = s"ScalarGen($tag, $gen)"
  }
  type EScalarGen = E[ScalarGen]

  object ScalarGen {

    def apply[T: ScalarTag: Arbitrary] = new ScalarGen[T] {
      val tag = implicitly
      val gen = implicitly[Arbitrary[T]].arbitrary
    }

    def apply[T](tag1: ScalarTag[T], gen1: Gen[T]) = new ScalarGen[T] {
      val tag = tag1
      val gen = gen1
    }

    val gen: Gen[EScalarGen] =
      Gen.oneOf(
        E(apply[Boolean]),
        E(apply[Byte]),
        E(apply[Short]),
        E(apply[Char]),
        E(apply[Int]),
        E(apply[Long]),
        E(apply[Float]),
        E(apply[Double]),
        E(apply[String]),
      )

    val genWithoutBool: Gen[EScalarGen] =
      Gen.oneOf(
        E(apply[Byte]),
        E(apply[Short]),
        E(apply[Char]),
        E(apply[Int]),
        E(apply[Long]),
        E(apply[Float]),
        E(apply[Double]),
        E(apply[String]),
      )

    val genWithoutNA: Gen[EScalarGen] =
      gen.map { esg =>
        implicit val tag = esg.value.tag
        E(ScalarGen(tag, esg.value.gen.filter(tag.notMissing)))
      }

    implicit val arbitrary: Arbitrary[E[ScalarGen]] = Arbitrary(gen)
  }

  /** Generated based on scalars of type `T`.
    */
  trait FromScalars[F[_]] {
    type Value[T1] = (ScalarTag[T1], F[T1])
    val tagged: E[Value]
    def tag: ScalarTag[tagged.T] = tagged.value._1
    def value: F[tagged.T] = tagged.value._2
    override def toString(): String = s"FromScalars(${tagged.value})"
  }

  object FromScalars {

    def apply[F[_], T1](tag1: ScalarTag[T1], value1: F[T1]) =
      new FromScalars[F] {
        val tagged = E[this.Value, T1]((tag1, value1))
      }

    implicit def arbitrary[F[_]](implicit
        arbEsg: Arbitrary[EScalarGen],
        fromScalarGen: FromScalarGen[F]
    ): Arbitrary[FromScalars[F]] = {
      Arbitrary(
        arbEsg.arbitrary.flatMap[FromScalars[F]](esg =>
          fromScalarGen(esg.value).map(f => FromScalars(esg.value.tag, f))
        )
      )
    }
  }

  /** This trait must be introduced because there is not support for
    * polymorphic or path-dependent function types in scala 2.
    */
  trait FromScalarGen[F[_]] {
    def apply[T](sg: ScalarGen[T]): Gen[F[T]]
  }
}
