package org.saddle.ops

import org.saddle.scalar.{
  ScalarTagDouble => stD,
  ScalarTagInt => stI,
  ScalarTagLong => stL,
  ScalarTagBool => stB
}

/** Contains implementations of primitive binary ops that are NA-aware
  *
  * Double primitive has NA bit pattern baked into its representation, but for
  * others we must check for the appropriate sentinel value.
  *
  * Note scala.Function2 is not specialized on Boolean inputs, only output
  */
object BinOps
    extends BinOpMat
    with BinOpMatInPlace
    with BinOpVec
    with BinOpVecInPlace
    with BinOpFrame
    with BinOpSeries {
  // ********************************************************
  // ** Concrete implementations necessary for specialization
  // ********************************************************

  // (D,D) => D

  implicit val powDD : BinOp[Power, Double, Double, Double] = new BinOp[Power, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      math.pow(a, b)
  }
  implicit val modDD : BinOp[Mod, Double, Double, Double] = new BinOp[Mod, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      a % b
  }
  implicit val addDD : BinOp[Add, Double, Double, Double] = new BinOp[Add, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      a + b
  }
  implicit val mulDD : BinOp[Multiply, Double, Double, Double] = new BinOp[Multiply, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      a * b
  }
  implicit val divDD : BinOp[Divide, Double, Double, Double] = new BinOp[Divide, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      a / b
  }
  implicit val subDD : BinOp[Subtract, Double, Double, Double] = new BinOp[Subtract, Double, Double, Double] {
    def apply(a: Double, b: Double): Double =
      a - b
  }

  // (D,L) => D

  implicit val powDL : BinOp[Power, Double, Long, Double] = new BinOp[Power, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else math.pow(a, b.toDouble)
  }
  implicit val modDL : BinOp[Mod, Double, Long, Double] = new BinOp[Mod, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addDL : BinOp[Add, Double, Long, Double] = new BinOp[Add, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulDL : BinOp[Multiply, Double, Long, Double] = new BinOp[Multiply, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divDL : BinOp[Divide, Double, Long, Double] = new BinOp[Divide, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subDL : BinOp[Subtract, Double, Long, Double] = new BinOp[Subtract, Double, Long, Double] {
    def apply(a: Double, b: Long): Double =
      if (stD.isMissing(a) || stL.isMissing(b)) stD.missing
      else a - b
  }

  // (L,D) => D

  implicit val powLD : BinOp[Power, Long, Double, Double] = new BinOp[Power, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else math.pow(a.toDouble, b)
  }
  implicit val modLD : BinOp[Mod, Long, Double, Double] = new BinOp[Mod, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addLD : BinOp[Add, Long, Double, Double] = new BinOp[Add, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulLD : BinOp[Multiply, Long, Double, Double] = new BinOp[Multiply, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divLD : BinOp[Divide, Long, Double, Double] = new BinOp[Divide, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subLD : BinOp[Subtract, Long, Double, Double] = new BinOp[Subtract, Long, Double, Double] {
    def apply(a: Long, b: Double): Double =
      if (stL.isMissing(a) || stD.isMissing(b)) stD.missing
      else a - b
  }

  // (I,D) => D

  implicit val powID : BinOp[Power, Int, Double, Double] = new BinOp[Power, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else math.pow(a, b)
  }
  implicit val modID : BinOp[Mod, Int, Double, Double] = new BinOp[Mod, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addID : BinOp[Add, Int, Double, Double] = new BinOp[Add, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulID : BinOp[Multiply, Int, Double, Double] = new BinOp[Multiply, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divID : BinOp[Divide, Int, Double, Double] = new BinOp[Divide, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subID : BinOp[Subtract, Int, Double, Double] = new BinOp[Subtract, Int, Double, Double] {
    def apply(a: Int, b: Double): Double =
      if (stI.isMissing(a) || stD.isMissing(b)) stD.missing
      else a - b
  }

  // (D,I) => D

  implicit val powDI : BinOp[Power, Double, Int, Double] = new BinOp[Power, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else math.pow(a, b)
  }
  implicit val modDI : BinOp[Mod, Double, Int, Double] = new BinOp[Mod, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a % b
  }
  implicit val addDI : BinOp[Add, Double, Int, Double] = new BinOp[Add, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a + b
  }
  implicit val mulDI : BinOp[Multiply, Double, Int, Double] = new BinOp[Multiply, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a * b
  }
  implicit val divDI : BinOp[Divide, Double, Int, Double] = new BinOp[Divide, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a / b
  }
  implicit val subDI : BinOp[Subtract, Double, Int, Double] = new BinOp[Subtract, Double, Int, Double] {
    def apply(a: Double, b: Int): Double =
      if (stD.isMissing(a) || stI.isMissing(b)) stD.missing
      else a - b
  }

  // (L,L) => L

  implicit val powLL : BinOp[Power, Long, Long, Long] = new BinOp[Power, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else math.pow(a.toDouble, b.toDouble).toLong
  }
  implicit val modLL : BinOp[Mod, Long, Long, Long] = new BinOp[Mod, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a % b
  }
  implicit val addLL : BinOp[Add, Long, Long, Long] = new BinOp[Add, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a + b
  }
  implicit val mulLL : BinOp[Multiply, Long, Long, Long] = new BinOp[Multiply, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a * b
  }
  implicit val divLL : BinOp[Divide, Long, Long, Long] = new BinOp[Divide, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a / b
  }
  implicit val subLL : BinOp[Subtract, Long, Long, Long] = new BinOp[Subtract, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a - b
  }
  implicit val andLL : BinOp[BitAnd, Long, Long, Long] = new BinOp[BitAnd, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a & b
  }
  implicit val orLL : BinOp[BitOr, Long, Long, Long] = new BinOp[BitOr, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a | b
  }
  implicit val xorLL : BinOp[BitXor, Long, Long, Long] = new BinOp[BitXor, Long, Long, Long] {
    def apply(a: Long, b: Long): Long =
      if (stL.isMissing(a) || stL.isMissing(b)) stL.missing
      else a ^ b
  }

  // (I,L) => L

  implicit val powIL : BinOp[Power, Int, Long, Long] = new BinOp[Power, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else math.pow(a, b.toDouble).toLong
  }
  implicit val modIL : BinOp[Mod, Int, Long, Long] = new BinOp[Mod, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a % b
  }
  implicit val addIL : BinOp[Add, Int, Long, Long] = new BinOp[Add, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a + b
  }
  implicit val mulIL : BinOp[Multiply, Int, Long, Long] = new BinOp[Multiply, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a * b
  }
  implicit val divIL : BinOp[Divide, Int, Long, Long] = new BinOp[Divide, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a / b
  }
  implicit val subIL : BinOp[Subtract, Int, Long, Long] = new BinOp[Subtract, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a - b
  }
  implicit val andIL : BinOp[BitAnd, Int, Long, Long] = new BinOp[BitAnd, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a & b
  }
  implicit val orIL : BinOp[BitOr, Int, Long, Long] = new BinOp[BitOr, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a | b
  }
  implicit val xorIL : BinOp[BitXor, Int, Long, Long] = new BinOp[BitXor, Int, Long, Long] {
    def apply(a: Int, b: Long): Long =
      if (stI.isMissing(a) || stL.isMissing(b)) stL.missing
      else a ^ b
  }

  // (L,I) => L

  implicit val powLI : BinOp[Power, Long, Int, Long] = new BinOp[Power, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else math.pow(a.toDouble, b).toLong
  }
  implicit val modLI : BinOp[Mod, Long, Int, Long] = new BinOp[Mod, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a % b
  }
  implicit val addLI : BinOp[Add, Long, Int, Long] = new BinOp[Add, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a + b
  }
  implicit val mulLI : BinOp[Multiply, Long, Int, Long] = new BinOp[Multiply, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a * b
  }
  implicit val divLI : BinOp[Divide, Long, Int, Long] = new BinOp[Divide, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a / b
  }
  implicit val subLI : BinOp[Subtract, Long, Int, Long] = new BinOp[Subtract, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a - b
  }
  implicit val andLI : BinOp[BitAnd, Long, Int, Long] = new BinOp[BitAnd, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a & b
  }
  implicit val orLI : BinOp[BitOr, Long, Int, Long] = new BinOp[BitOr, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a | b
  }
  implicit val xorLI : BinOp[BitXor, Long, Int, Long] = new BinOp[BitXor, Long, Int, Long] {
    def apply(a: Long, b: Int): Long =
      if (stL.isMissing(a) || stI.isMissing(b)) stL.missing
      else a ^ b
  }

  // (I,I) => I

  implicit val powII : BinOp[Power, Int, Int, Int] = new BinOp[Power, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else math.pow(a, b).toInt
  }
  implicit val modII : BinOp[Mod, Int, Int, Int] = new BinOp[Mod, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a % b
  }
  implicit val addII : BinOp[Add, Int, Int, Int] = new BinOp[Add, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a + b
  }
  implicit val mulII : BinOp[Multiply, Int, Int, Int] = new BinOp[Multiply, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a * b
  }
  implicit val divII : BinOp[Divide, Int, Int, Int] = new BinOp[Divide, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a / b
  }
  implicit val subII : BinOp[Subtract, Int, Int, Int] = new BinOp[Subtract, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a - b
  }
  implicit val andII : BinOp[BitAnd, Int, Int, Int] = new BinOp[BitAnd, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a & b
  }
  implicit val orII : BinOp[BitOr, Int, Int, Int] = new BinOp[BitOr, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a | b
  }
  implicit val xorII : BinOp[BitXor, Int, Int, Int] = new BinOp[BitXor, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a ^ b
  }
  implicit val shlII : BinOp[BitShl, Int, Int, Int] = new BinOp[BitShl, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a << b
  }
  implicit val shrII : BinOp[BitShr, Int, Int, Int] = new BinOp[BitShr, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a >> b
  }
  implicit val ushrII : BinOp[BitUShr, Int, Int, Int] = new BinOp[BitUShr, Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (stI.isMissing(a) || stI.isMissing(b)) stI.missing
      else a >>> b
  }

  // (Bool, Bool) => Bool ops

  implicit val andBB : BinOp[AndOp, Boolean, Boolean, Boolean] = new BinOp[AndOp, Boolean, Boolean, Boolean] {
    def apply(a: Boolean, b: Boolean) = a && b
  }
  implicit val orBB : BinOp[OrOp, Boolean, Boolean, Boolean] = new BinOp[OrOp, Boolean, Boolean, Boolean] {
    def apply(a: Boolean, b: Boolean) = a || b
  }
  implicit val xorBB : BinOp[XorOp, Boolean, Boolean, Boolean] = new BinOp[XorOp, Boolean, Boolean, Boolean] {
    def apply(a: Boolean, b: Boolean) = a && b || !a && !b
  }

  /* comparisons ops */

  // >
  implicit val gtDD : BinOp[GtOp, Double, Double, Boolean] = new BinOp[GtOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtDL : BinOp[GtOp, Double, Long, Boolean] = new BinOp[GtOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtDI : BinOp[GtOp, Double, Int, Boolean] = new BinOp[GtOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtLD : BinOp[GtOp, Long, Double, Boolean] = new BinOp[GtOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtLL : BinOp[GtOp, Long, Long, Boolean] = new BinOp[GtOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtLI : BinOp[GtOp, Long, Int, Boolean] = new BinOp[GtOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtID : BinOp[GtOp, Int, Double, Boolean] = new BinOp[GtOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtII : BinOp[GtOp, Int, Int, Boolean] = new BinOp[GtOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a > b)
    }
  implicit val gtBB : BinOp[GtOp, Boolean, Boolean, Boolean] = new BinOp[GtOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a > b)
    }

  // <

  implicit val ltDD : BinOp[LtOp, Double, Double, Boolean] = new BinOp[LtOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltDL : BinOp[LtOp, Double, Long, Boolean] = new BinOp[LtOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltDI : BinOp[LtOp, Double, Int, Boolean] = new BinOp[LtOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltLD : BinOp[LtOp, Long, Double, Boolean] = new BinOp[LtOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltLL : BinOp[LtOp, Long, Long, Boolean] = new BinOp[LtOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltLI : BinOp[LtOp, Long, Int, Boolean] = new BinOp[LtOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltID : BinOp[LtOp, Int, Double, Boolean] = new BinOp[LtOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltII : BinOp[LtOp, Int, Int, Boolean] = new BinOp[LtOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a < b)
    }
  implicit val ltBB : BinOp[LtOp, Boolean, Boolean, Boolean] = new BinOp[LtOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a < b)
    }

  // ==

  implicit val eqDD : BinOp[EqOp, Double, Double, Boolean] = new BinOp[EqOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqDL : BinOp[EqOp, Double, Long, Boolean] = new BinOp[EqOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqDI : BinOp[EqOp, Double, Int, Boolean] = new BinOp[EqOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqLD : BinOp[EqOp, Long, Double, Boolean] = new BinOp[EqOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqLL : BinOp[EqOp, Long, Long, Boolean] = new BinOp[EqOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqLI : BinOp[EqOp, Long, Int, Boolean] = new BinOp[EqOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a == b)
    }
      implicit val eqID : BinOp[EqOp, Int, Double, Boolean] = new BinOp[EqOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqII : BinOp[EqOp, Int, Int, Boolean] = new BinOp[EqOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a == b)
    }
  implicit val eqBB : BinOp[EqOp, Boolean, Boolean, Boolean] = new BinOp[EqOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a == b)
    }

  // !=

  implicit val neqDD : BinOp[NeqOp, Double, Double, Boolean] = new BinOp[NeqOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqDL : BinOp[NeqOp, Double, Long, Boolean] = new BinOp[NeqOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a != b)
    } 
    implicit val neqDI:BinOp[NeqOp, Double, Int, Boolean] =    new BinOp[NeqOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqLD : BinOp[NeqOp, Long, Double, Boolean] = new BinOp[NeqOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqLL : BinOp[NeqOp, Long, Long, Boolean] = new BinOp[NeqOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqLI : BinOp[NeqOp, Long, Int, Boolean] = new BinOp[NeqOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqID : BinOp[NeqOp, Int, Double, Boolean] = new BinOp[NeqOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqII : BinOp[NeqOp, Int, Int, Boolean] = new BinOp[NeqOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a != b)
    }
  implicit val neqBB : BinOp[NeqOp, Boolean, Boolean, Boolean] = new BinOp[NeqOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a != b)
    }

  // >=

  implicit val gteDD : BinOp[GteOp, Double, Double, Boolean] = new BinOp[GteOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteDL : BinOp[GteOp, Double, Long, Boolean] = new BinOp[GteOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteDI : BinOp[GteOp, Double, Int, Boolean] = new BinOp[GteOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteLD : BinOp[GteOp, Long, Double, Boolean] = new BinOp[GteOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteLL : BinOp[GteOp, Long, Long, Boolean] = new BinOp[GteOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteLI : BinOp[GteOp, Long, Int, Boolean] = new BinOp[GteOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteID : BinOp[GteOp, Int, Double, Boolean] = new BinOp[GteOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteII : BinOp[GteOp, Int, Int, Boolean] = new BinOp[GteOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a >= b)
    }
  implicit val gteBB : BinOp[GteOp, Boolean, Boolean, Boolean] = new BinOp[GteOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a >= b)
    }

  // <=

  implicit val lteDD : BinOp[LteOp, Double, Double, Boolean] = new BinOp[LteOp, Double, Double, Boolean] {
      def apply(a: Double, b: Double) =
        if (stD.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteDL : BinOp[LteOp, Double, Long, Boolean] = new BinOp[LteOp, Double, Long, Boolean] {
      def apply(a: Double, b: Long) =
        if (stD.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteDI : BinOp[LteOp, Double, Int, Boolean] = new BinOp[LteOp, Double, Int, Boolean] {
      def apply(a: Double, b: Int) =
        if (stD.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteLD : BinOp[LteOp, Long, Double, Boolean] = new BinOp[LteOp, Long, Double, Boolean] {
      def apply(a: Long, b: Double) =
        if (stL.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteLL : BinOp[LteOp, Long, Long, Boolean] = new BinOp[LteOp, Long, Long, Boolean] {
      def apply(a: Long, b: Long) =
        if (stL.isMissing(a) || stL.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteLI : BinOp[LteOp, Long, Int, Boolean] = new BinOp[LteOp, Long, Int, Boolean] {
      def apply(a: Long, b: Int) =
        if (stL.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteID : BinOp[LteOp, Int, Double, Boolean] = new BinOp[LteOp, Int, Double, Boolean] {
      def apply(a: Int, b: Double) =
        if (stI.isMissing(a) || stD.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteII : BinOp[LteOp, Int, Int, Boolean] = new BinOp[LteOp, Int, Int, Boolean] {
      def apply(a: Int, b: Int) =
        if (stI.isMissing(a) || stI.isMissing(b)) stB.missing
        else (a <= b)
    }
  implicit val lteBB : BinOp[LteOp, Boolean, Boolean, Boolean] = new BinOp[LteOp, Boolean, Boolean, Boolean] {
      def apply(a: Boolean, b: Boolean) =
        if (stB.isMissing(a) || stB.isMissing(b)) stB.missing
        else (a <= b)
    }
}
