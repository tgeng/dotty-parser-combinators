import io.github.tgeng.parse.string.{_, given _}
import io.github.tgeng.parse._
import scala.language.implicitConversions

object Main {
  def main(args: Array[String]): Unit = {
    val a = P { "a" }
    val b = P { "b" }
    // val ab = a.flatMap(a => b.map(b => a + b))
    val ab = P {
      for {
        a <- a
        b <- b
      } yield a + b
    }
    println(ab.parse("a"))
  }
}
