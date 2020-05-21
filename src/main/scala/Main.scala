import io.github.tgeng.parse.string.{_, given _}
import io.github.tgeng.parse._
import scala.language.implicitConversions

object Main {
  def main(args: Array[String]): Unit = {
    val ab = P {
      for {
        a <- "a"
        b <- "b"
      } yield a + b
    }
    println(ab.parse("a"))
  }
}
