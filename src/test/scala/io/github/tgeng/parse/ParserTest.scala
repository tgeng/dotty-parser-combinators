package io.github.tgeng.parse;

import scala.collection.IndexedSeq
import scala.language.implicitConversions
import org.junit.Assert._
import org.junit.Test

class ParserTest {
  import io.github.tgeng.parse.string.{_, given}
  import io.github.tgeng.parse._
  @Test
  def `basic parsers` = {
    testing(pure(())) {
      "" ~> (())
    }
    testing(pure((1))) {
      "" ~> 1
    }
    testing(position) {
      "" ~> 0
    }
    testing(empty) {
      "" ~> (())
      "abc" ~> (())
    }
    testing(any) {
      "abc" ~> 'a'
      "" ~^ "0: <any>"
    }
    testing(eof) {
      "" ~> (())
      "abc" ~^ "0: <eof>"
    }
    testing(skip) {
      "abc" ~> (())
      "" ~^ "0: <skip>"
    }

    testing(anyOf("abc")) {
      "ccc" ~> 'c'
      "" ~^ "0: <anyOf{a, b, c}>"
    }

    testing('c') {
      "c" ~> 'c'
      "change" ~> 'c'
      "abc" ~^ "0: 'c'"
    }

    testing("abc") {
      "abc" ~> "abc"
      "abcdef" ~> "abc"
      "def" ~^ """0: "abc""""
    }

    testing("[0-9]+".r) {
      "123" ~> "123"
      "123abc" ~> "123"
      "abc" ~^ """0: /[0-9]+/"""
    }
  }

  @Test
  def `or operator` = {
    testing("a" | "b" | "c" | "d") {
      "a" ~> "a"
      "b" ~> "b"
      "x" ~^ """
        0: "d"
        0: "a" | "b" | "c" | "d"
      """
    }
  }

  @Test
  def `commit operator and |` = {
    val oct = "0" >> !"[0-7]+".r withName "oct"
    val hex = "0x" >> !"[0-9a-f]+".r withName "hex"
    val octOrHex = hex | oct | ".*".r

    testing(octOrHex) {
      "0xaaf" ~> "aaf"
      "0123" ~> "123"
      "0abc" ~^ """
        1: !/[0-7]+/
        0: oct := "0" >> !/[0-7]+/
        0: hex | oct | /.*/
      """
    }
  }

  @Test
  def `commit operator and *` = {
    val p = ":" >> !"\\w+".r
    testing(p*) {
      ":abc" ~> Seq("abc")
      ":abc:def" ~> Seq("abc", "def" )
      ":abc:?" ~^ """
        5: !/\w+/
        4: ":" >> !/\w+/
        0: (":" >> !/\w+/)*
      """
    }
  }

  @Test
  def `not operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_))
    val keyword = ("def" | "class" | "val") << not(!alphabet)

    testing(keyword) {
      "def" ~> "def"
      "class" ~> "class"
      "val" ~> "val"
      "definition" ~^ """
        3: not !<satisfy>
        0: ("def" | "class" | "val") << not !<satisfy>
      """
    }
  }

  @Test
  def `and operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_)) withName "alphabet"
    val digit = satisfy[Char](Character.isDigit(_)) withName "digit"
    val keyword = ("def" | "class" | "val") << not(!alphabet) withName "keyword"
    val identifier = "\\w+".r & not(keyword) & not(digit) // not starting with digit

    testing(identifier) {
      "abc" ~> "abc"
      "definition" ~> "definition"
      "def" ~^ """
        0: not keyword
        0: /\w+/ & not keyword & not digit
      """
      "123abc" ~^ """
        0: not digit
        0: /\w+/ & not keyword & not digit
      """
    }
  }

  @Test
  def `suffix operators` = {
    val abc = parser("abc")
    testing(abc*) {
      "abcabcabd" ~> Seq("abc", "abc" )
      "def" ~> Seq[String]()
    }
    testing(abc+) {
      "abcabcabd" ~> Seq("abc", "abc" )
      "def" ~^ """
        0: "abc"
        0: "abc"+
      """
    }
    testing(abc?) {
      "abcabcabd".~>[Char, Option[String]](Some("abc"))
      "def".~>[Char, Option[String]](None)
    }
  }

  @Test
  def `repeat operator` = {
    testing(3 * "abc") {
      "abcabcabc" ~> Seq("abc", "abc", "abc" )
      "abcabcabcabc" ~> Seq("abc", "abc", "abc" )
      "abcabc" ~^ """
        6: "abc"
        0: 3 * "abc"
      """
    }
  }

  @Test
  def `sepBy operator` = {
    val word = "\\w+".r withName "word"
    testing(word sepBy1 ',') {
      "abc" ~> Seq("abc" )
      "abc,def" ~> Seq("abc", "def" )
      "~~" ~^ """
        0: word := /\w+/
        0: word sepBy1 ','
      """
    }
    testing(word sepBy ',') {
      "abc" ~> Seq("abc" )
      "abc,def" ~> Seq("abc", "def" )
      "~~" ~> Seq[String]()
    }
    testing(word.sepByN(3)(',')) {
      "ab,cd,ef" ~> Seq("ab", "cd", "ef" )
      "ab,cd,ef,gh" ~> Seq("ab", "cd", "ef" )
      "ab,cd" ~^ """
        5: ','
        5: ',' >> word
        2: 2 * (',' >> word)
        0: word sepByN(3) ','
      """
    }
  }

  @Test
  def `prefix and suffix` = {
    val word = "\\w+".r withName "word"
    testing('(' >> word << ')') {
    "(abc)" ~> "abc"
    "(abc)def" ~> "abc"
    "()" ~^ """
      1: word := /\w+/
      0: '(' >> word << ')'
    """
    "abc" ~^ """
      0: '('
      0: '(' >> word << ')'
    """
    }
  }

  @Test
  def `apply operator` = {
    val spaces = parser(' ')*
    val number = ("[0-9]+".r << spaces).map(_.toInt) withName "number"
    testing(pure((a: Int, b: Int) => a + b, "Sum2") <*> (number, number)) {
      "12 34" ~> 46
      "12 ab" ~^ """
        3: /[0-9]+/
        3: number := /[0-9]+/ << ' '*
        0: Sum2 <*> (number, number)
      """
    }
    testing(pure((a: Int, b: Int, c: Int) => a + b + c, "Sum3") <*> (number, number, number)) {
      "12 34 56" ~> 102
      "12 34" ~^ """
        5: /[0-9]+/
        5: number := /[0-9]+/ << ' '*
        0: Sum3 <*> (number, number, number)
      """
    }
    testing(pure((a: Int, b: Int, c: Int, d: Int) => a + b + c + d, "Sum4") <*> (number, number, number, number)) {
    "12 34 56 78" ~> 180
    "12 34 56 " ~^ """
      9: /[0-9]+/
      9: number := /[0-9]+/ << ' '*
      0: Sum4 <*> (number, number, number, number)
    """
    }
  }

  @Test
  def `chainedLeftBy and chainedRightBy` = {
    val op = ("+" | "-").map(op => ((a: String, b: String) => "(" + a + op + b + ")"))
    testing(".".r chainedLeftBy op) {
      "a+b+c" ~> "((a+b)+c)"
    }
    testing(".".r chainedRightBy op) {
      "a+b+c" ~> "(a+(b+c))"
    }
  }

  val realNumber : Parser[Double] = for {
    sign <- ('-'?).map(_.map(_ => -1).getOrElse(1))
    beforePoint <- "[0-9]+".r
    afterPoint <- (('.' >> !"[0-9]+".r)?)
      .map(_.map(s => s.toInt / math.pow(10.0, s.size))
            .getOrElse(0.0))
  } yield sign * (beforePoint.toInt + afterPoint)

  @Test
  def `prepend append concat` = {
    val a = parser('a')
    val b = parser('b')
    val c = parser('c')

    testing(a +:+ b) {
      "abc".~>[Char, IndexedSeq[Char]]("ab")
      "a" ~^ """
        1: 'b'
        0: 'a' +:+ 'b'
      """
    }
    val ab = a +:+ b
    testing(c +: ab) {
      "cab".~>[Char, IndexedSeq[Char]]("cab")
      "ab" ~^ """
        0: 'c'
        0: 'c' +: 'a' +:+ 'b'
      """
    }
    testing(ab :+ c) {
      "abc".~>[Char, IndexedSeq[Char]]("abc")
      "abd" ~^ """
        2: 'c'
        0: 'a' +:+ 'b' :+ 'c'
      """
    }
    testing(ab ++ ab) {
      "abab".~>[Char, IndexedSeq[Char]]("abab")
      "abc" ~^ """
        2: 'a'
        0: 'a' +:+ 'b' ++ 'a' +:+ 'b'
      """
    }
  }

  @Test
  def `parse real number` = testing(realNumber) {
    "2" ~> 2.0
    "-50" ~> -50.0
    "-50.25" ~> -50.25
    "30.5" ~> 30.5
    "123a" ~> 123.0
    "abc" ~^  """
      0: /[0-9]+/
      0: '-'? /[0-9]+/ <?>
    """
    "4." ~^  """
      2: !/[0-9]+/
      1: '.' >> !/[0-9]+/
      1: ('.' >> !/[0-9]+/)?
      0: '-'? /[0-9]+/ ('.' >> !/[0-9]+/)?
    """
  }

  @Test
  def `calculator` = {
    val spaces = parser(' ')*
    val plus = ('+'!) as ((a: Double, b: Double) => a + b) withName "+"
    val minus = ('-'!) as ((a: Double, b: Double) => a - b) withName "-"
    val multiply = ('*'!) as ((a: Double, b: Double) => a * b) withName "*"
    val divide = ('/'!) as ((a: Double, b: Double) => a / b) withName "/"

    def sumExpr: Parser[Double] =
      prodExpr.chainedLeftBy(spaces >> (plus | minus) << spaces) withName "sumExpr"

    def prodExpr: Parser[Double] =
      term.chainedLeftBy(spaces >> (multiply | divide) << spaces) withName "prodExpr"

    def term: Parser[Double] =
      realNumber |
      "(" >> spaces >> sumExpr << spaces << ")" withName "term"

    testing(sumExpr << eof) {
      "1" ~> 1.0
      "1+2" ~> 3.0
      "1 + 2" ~> 3.0
      "1 - 2" ~> -1.0
      "1 * 2" ~> 2.0
      "1 / 2" ~> 0.5
      "1 + 2 * 3" ~> 7.0
      "1 * 2 + 3" ~> 5.0
      "1 * 2 + 3" ~> 5.0
      "2 * (3 + 4)" ~> 14.0
      "2 * ( 3 + 4 )" ~> 14.0
      "1 + 2 * 3 + (4 - 5) * 6" ~> 1.0
    }
  }

}

private def testing[I, T](parser: ParserT[I, T])(block: (given p: ParserT[I, T]) => Unit) = {
  block(given parser)
}

private def [I, T](input: IndexedSeq[I]) ~> (expected: T)(using parser: ParserT[I, T]) = {
  parser.parse(input) match {
    case Right(actual) => (actual == expected) match {
      case true => ()
      case false => fail(
        getFailMessagePrefix(parser, input) + "expect output to be\n  " +
        expected.toString.indented(2) + "\nbut actual output is\n  " +
        actual.toString.indented(2) + "\n"
        )
    }
    case Left(e) => fail(
      getFailMessagePrefix(parser, input) + "expect output to be\n  " +
        expected.toString.indented(2) + "\nbut parsing fails with message\n  " +
        e.toString.indented(2) + "\n"
    )
  }
}

private def [I, T](input: IndexedSeq[I]) ~^ (errorMessage: String)(using parser: ParserT[I, T]) = {
  val t = parser.parse(input)
  val trimmedMessage = errorMessage.trim.replaceAll("\n +", "\n")
  t match {
     case Right(t) => fail(getFailMessagePrefix(parser, input) +
       s"expect parsing to fail but it succeeds with\n  ${t.toString.indented(2)}\n")
     case Left(e) => e.toString() == trimmedMessage match {
       case false => fail(getFailMessagePrefix(parser, input) +
         s"expect parsing to fail with message\n  ${trimmedMessage.indented(2)}\nbut it fails with message\n  ${e.toString().indented(2)}\n")
       case _ => ()
     }
  }
}

private def getFailMessagePrefix(parser: ParserT[?, ?], input: Any) =
  s"\nWith ${parser.toString} and given input\n  " +
  input.toString.indented(2) + "\n"

def (s: String | Null) indented(count: Int) =
  if (s == null) null else s.replaceAll("\n", "\n" + (" " * count))