import org.saddle._
import org.saddle.order._
import org.saddle.macros.BinOps._
// import org.saddle.ops.BinOps._

object M extends App {
  0 until 100000 foreach { _ =>
    val v1 = vec.zeros(100000)
    val v2 = vec.ones(100000)
    v1 += v2
  }
}
