package io.github.tgeng.parse;

import scala.collection.IndexedSeq
import scala.language.implicitConversions
import org.junit.Assert._
import org.junit.Test

class ParserTest {
  import io.github.tgeng.parse.string.{_, given _}
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
    testing(nothing) {
      "" ~> (())
      "abc" ~> (())
    }
    testing(any) {
      "abc" ~> 'a'
      "" ~^ """
        |
        |^
        |0: <any>
      """
    }
    testing(eof) {
      "" ~> (())
      "abc" ~^ """
        |abc
        |^
        |0: <eof>
      """
    }
    testing(skip) {
      "abc" ~> (())
      "" ~^ """
        |
        |^
        |0: <skip>
      """
    }

    testing(anyOf("abc")) {
      "ccc" ~> 'c'
      "" ~^ """
        |
        |^
        |0: <anyOf{a, b, c}>
      """
    }

    testing('c') {
      "c" ~> 'c'
      "change" ~> 'c'
      "abc" ~^ """
        |abc
        |^
        |0: 'c'
      """
    }

    testing("abc") {
      "abc" ~> "abc"
      "abcdef" ~> "abc"
      "def" ~^ """
        |def
        |^
        |0: "abc"
      """
    }

    testing("[0-9]+".rp) {
      "123" ~> "123"
      "123abc" ~> "123"
      "abc" ~^ """
        |abc
        |^
        |0: /[0-9]+/
      """
    }
  }

  @Test
  def `test lineColumn` = {
    testing(lineColumn) {
      "" ~> (0, 0)
      "abc" ~> (0, 0)
      "\n" ~> (0, 0)
    }

    testing(whitespaces >> lineColumn) {
      "" ~> (0, 0)
      "  " ~> (0, 2)
      "     " ~> (0, 5)
      "\n" ~> (1, 0)
      "\n " ~> (1, 1)
      "\n\n " ~> (2, 1)
      "\n\r\n\n  " ~> (3, 2)
      "\n  \n " ~> (2, 1)
      "\r\n  \r\n " ~> (2, 1)
    }
  }

  @Test
  def `test column` = {
    testing(whitespaces >> column) {
      "" ~> 0
      "abc" ~> 0
      "  a" ~> 2
      "\n" ~> 0
      "\n " ~> 1
      "\r\n  " ~> 2
    }
  }

  @Test
  def `test blankLine` = {
    testing(blankLine.* >> lineColumn) {
      " " ~> (0, 0)
      "\n " ~> (1, 0)
      "  \n " ~> (1, 0)
      "  \n  \n " ~> (2, 0)
    }
  }

  @Test
  def `for comprehension` = {
    val ab = P {
      for {
        a <- "a"
        b <- "b"
      } yield a + b
    }

    testing(ab) {
      "ab" ~> "ab"
      "a" ~^ """
        |a
        | ^
        |1: "b"
        |0: <ab> := "a" "b"
      """
    }
  }

  @Test
  def `or operator` = {
    testing("a" | "b" | "c" | "d") {
      "a" ~> "a"
      "b" ~> "b"
      "x" ~^ """
        |x
        |^
        |0: "d"
        |0: "a" | "b" | "c" | "d"
      """
    }
  }

  @Test
  def `commit operator and |` = {
    val oct = "0" >> commitBefore("[0-7]+".rp) withName "oct"
    val hex = "0x" >> commitBefore("[0-9a-f]+".rp) withName "hex"
    val octOrHex = hex | oct | ".*".rp

    testing(octOrHex) {
      "0xaaf" ~> "aaf"
      "0123" ~> "123"
      "0abc" ~^ """
        |0abc
        | ^
        |1: /[0-7]+/
        |0: oct := "0" >> !/[0-7]+/
        |0: hex | oct | /.*/
      """
    }
  }

  @Test
  def `commit operator and *` = {
    val p = ":" >>! "\\w+".rp
    testing(p*) {
      ":abc" ~> Seq("abc")
      ":abc:def" ~> Seq("abc", "def" )
      ":abc:?" ~> Seq("abc")
    }
  }

  @Test
  def `not operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_))
    val keyword = ("def" | "class" | "val") << not(alphabet)

    testing(keyword) {
      "def" ~> "def"
      "class" ~> "class"
      "val" ~> "val"
      "definition" ~^ """
        |definition
        |   ^
        |3: not <satisfy>
        |0: ("def" | "class" | "val") << not <satisfy>
      """
    }
  }

  @Test
  def `and operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_)) withName "alphabet"
    val digit = satisfy[Char](Character.isDigit(_)) withName "digit"
    val keyword = ("def" | "class" | "val") << not(alphabet) withName "keyword"
    val identifier = "\\w+".rp & not(keyword) & not(digit) // not starting with digit

    testing(identifier) {
      "abc" ~> "abc"
      "definition" ~> "definition"
      "def" ~^ """
        |def
        |^
        |0: not keyword
        |0: /\w+/ & not keyword & not digit
      """
      "123abc" ~^ """
        |123abc
        |^
        |0: not digit
        |0: /\w+/ & not keyword & not digit
      """
    }
  }

  @Test
  def `suffix operators` = {
    val abc = p("abc")
    testing(abc*) {
      "abcabcabd" ~> Seq("abc", "abc" )
      "def" ~> Seq[String]()
    }
    testing(abc+) {
      "abcabcabd" ~> Seq("abc", "abc" )
      "def" ~^ """
        |def
        |^
        |0: "abc"
        |0: "abc"+
      """
    }
    testing(abc?) {
      "abcabcabd".~>[Option[String]](Some("abc"))
      "def".~>[Option[String]](None)
    }
  }

  @Test
  def `repeat operator` = {
    testing(3 * "abc") {
      "abcabcabc" ~> Seq("abc", "abc", "abc" )
      "abcabcabcabc" ~> Seq("abc", "abc", "abc" )
      "abcabc" ~^ """
        |abcabc
        |      ^
        |6: "abc"
        |0: 3 * "abc"
      """
    }
  }

  @Test
  def `sepBy operator` = {
    val word = "\\w+".rp withName "word"
    testing(word sepBy1 ',') {
      "abc" ~> Seq("abc" )
      "abc,def" ~> Seq("abc", "def" )
      "~~" ~^ """
        |~~
        |^
        |0: /\w+/
        |0: word := /\w+/
        |0: word sepBy1 ','
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
        |ab,cd
        |     ^
        |5: ','
        |0: word sepByN(3) ','
      """
    }
  }

  @Test
  def `prefix and suffix` = {
    val word = "\\w+".rp withName "word"
    testing('(' >> word << ')') {
    "(abc)" ~> "abc"
    "(abc)def" ~> "abc"
    "()" ~^ """
      |()
      | ^
      |1: /\w+/
      |1: word := /\w+/
      |0: '(' >> word << ')'
    """
    "abc" ~^ """
      |abc
      |^
      |0: '('
      |0: '(' >> word << ')'
    """
    }
  }

  @Test
  def `apply operator` = {
    val spaces = p(' ')*
    val number = ("[0-9]+".rp << spaces).map(_.toInt) withName "number"
    testing(pure((a: Int, b: Int) => a + b, "Sum2") <*> (number, number)) {
      "12 34" ~> 46
      "12 ab" ~^ """
        |12 ab
        |   ^
        |3: /[0-9]+/
        |3: number := /[0-9]+/ << ' '*
        |0: Sum2 <*> (number, number)
      """
    }
    testing(pure((a: Int, b: Int, c: Int) => a + b + c, "Sum3") <*> (number, number, number)) {
      "12 34 56" ~> 102
      "12 34" ~^ """
        |12 34
        |     ^
        |5: /[0-9]+/
        |5: number := /[0-9]+/ << ' '*
        |0: Sum3 <*> (number, number, number)
      """
    }
    testing(pure((a: Int, b: Int, c: Int, d: Int) => a + b + c + d, "Sum4") <*> (number, number, number, number)) {
      "12 34 56 78" ~> 180
      "12 34 56 " ~^ """
        |12 34 56 
        |         ^
        |9: /[0-9]+/
        |9: number := /[0-9]+/ << ' '*
        |0: Sum4 <*> (number, number, number, number)
      """
    }
  }

  @Test
  def `chainedLeftBy and chainedRightBy` = {
    val op = ("+" | "-").map(op => ((a: String, b: String) => "(" + a + op + b + ")"))
    testing(".".rp chainedLeftBy op) {
      "a+b+c" ~> "((a+b)+c)"
    }
    testing(".".rp chainedRightBy op) {
      "a+b+c" ~> "(a+(b+c))"
    }
  }

  @Test
  def `prepend append concat` = {
    val a = p('a')
    val b = p('b')
    val c = p('c')

    testing(a +:+ b) {
      "abc".~>[IndexedSeq[Char]]("ab")
      "a" ~^ """
        |a
        | ^
        |1: 'b'
        |0: 'a' +:+ 'b'
      """
    }
    val ab = a +:+ b
    testing(c +: ab) {
      "cab".~>[IndexedSeq[Char]]("cab")
      "ab" ~^ """
        |ab
        |^
        |0: 'c'
        |0: 'c' +: 'a' +:+ 'b'
      """
    }
    testing(ab :+ c) {
      "abc".~>[IndexedSeq[Char]]("abc")
      "abd" ~^ """
        |abd
        |  ^
        |2: 'c'
        |0: 'a' +:+ 'b' :+ 'c'
      """
    }
    testing(ab ++ ab) {
      "abab".~>[IndexedSeq[Char]]("abab")
      "abc" ~^ """
        |abc
        |  ^
        |2: 'a'
        |0: 'a' +:+ 'b' ++ 'a' +:+ 'b'
      """
    }
  }

  @Test
  def `lifting` = {
    val abParser = lift(Vector("a".p, "b".p))
    testing(abParser) {
      "ab" ~> Vector("a", "b")
      "cd" ~^ """
        |cd
        |^
        |0: "a"
        |0: lift{"a", "b"}
      """
    }

    val abcParser = lift("a".p, "b".p, "c".p)
    testing(abcParser) {
      "abc" ~> ("a", "b", "c")
      "abd" ~^ """
        |abd
        |  ^
        |2: "c"
        |0: ("a", "b", "c")
      """
    }
  }

  @Test
  def `calculator` = {
    val spaces = p(' ')*
    val plus = commitAfter('+') as ((a: Double, b: Double) => a + b) withName "+"
    val minus = commitAfter('-') as ((a: Double, b: Double) => a - b) withName "-"
    val multiply = commitAfter('*') as ((a: Double, b: Double) => a * b) withName "*"
    val divide = commitAfter('/') as ((a: Double, b: Double) => a / b) withName "/"

    def sumExpr: Parser[Double] =
      prodExpr.chainedLeftBy(spaces >> (plus | minus) << spaces) withName "sumExpr"

    def prodExpr: Parser[Double] =
      term.chainedLeftBy(spaces >> (multiply | divide) << spaces) withName "prodExpr"

    def term: Parser[Double] =
      double |
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

  @Test
  def `simple string parsers` = {
    testing(space) {
      " " ~> ' '
    }
    testing(spaces) {
      "  a" ~> "  "
      "" ~> ""
      " \t" ~> " "
    }
    testing(whitespace) {
      " " ~> ' '
      "\t" ~> '\t'
    }
    testing(whitespaces) {
      " \t \n" ~> (())
      "" ~> (())
    }

    testing(lf) {
      "\n" ~> '\n'
    }
    testing(cr) {
      "\r" ~> '\r'
    }
    testing(crlf) {
      "\r\n" ~> "\r\n"
    }
    testing(newline) {
      "\r\n" ~> (())
      "\n" ~> (())
    }

    testing(upper) {
      "A" ~> 'A'
      "a" ~^ """
        |a
        |^
        |0: <upper>
      """
    }
    testing(lower) {
      "a" ~> 'a'
      "A" ~^ """
        |A
        |^
        |0: <lower>
      """
    }
    testing(digit) {
      "7" ~> '7'
      "a" ~^ """
        |a
        |^
        |0: <digit>
      """
    }
    testing(alphaNum) {
      "a" ~> 'a'
      "A" ~> 'A'
      "1" ~> '1'
    }

    testing(int) {
      "123" ~> 123
      "+12" ~> 12
      "-23" ~> -23
      "abc" ~^ """
        |abc
        |^
        |0: <int>
      """
    }
    testing(double) {
      "+2" ~> 2.0
      "-50" ~> -50.0
      "-50.25" ~> -50.25
      "30.5" ~> 30.5
      "123a" ~> 123.0
      "4." ~> 4.0
      ".5" ~> 0.5
      "abc" ~^  """
        |abc
        |^
        |0: <double>
      """
    }
  }

  @Test
  def `test indented` = testing(withIndent(2) { whitespaces >> lineColumn }) {
    "" ~> (0, 0)
    " " ~> (0, 1)
    " \n" ~> (0, 1)
    " \n " ~> (0, 1)
    " \n  " ~> (1, 2)
    " \n\n  " ~> (2, 2)
    "     \n \n  " ~> (2, 2)
  }

  @Test
  def `test aligned` = testing(aligned{"do " >> ("\\w+".rp sepBy1 (someLines << spaces))}) {
    """do foo
      |bar
      |quux
    """.stripMargin ~> Seq("foo", "bar", "quux")

    """  do foo
      |  bar
      |  quux
      |next
    """.stripMargin ~> Seq("foo", "bar", "quux")

    """  do foo
      |     bar
      |     quux
      |next
    """.stripMargin ~> Seq("foo", "bar", "quux")
  }

  @Test
  def `test quoted` = testing(quoted(quoteSymbol = '\'', escapeSymbol = '$')) {
    "'abc'" ~> "abc"
    "'$t'" ~> "\t"
    "'$'quoted$$string$''" ~> "'quoted$string'"
    "abc" ~^ """
      |abc
      |^
      |0: <'-quoted>
    """
    "'abc" ~^ """
      |'abc
      |^
      |0: <'-quoted>
    """
  }

  @Test
  def `test fail` = testing(fail("blah")) {
    "" ~^ """
      |
      |^
      |blah
      |0: <failure>
    """
    "yoo" ~^ """
      |yoo
      |^
      |blah
      |0: <failure>
    """
  }

  @Test
  def `test withErrorMessage` = testing("abc".rp.withErrorMessage("should match 'abc'")) {
    "abc" ~> "abc"
    "def" ~^ """
      |def
      |^
      |should match 'abc'
      |0: /abc/
    """
  }

  @Test 
  def `test withFilter` = testing(".*".rp.withFilter(_ == "foo")) {
    "foo" ~> "foo"
    "foobar" ~^ """
      |foobar
      |^
      |0: /.*/ satisfying some custom predicate
    """
  }

  import JValue._

  //          ┌ a macro that names the parser according to the enclosing definition, For example, in
  //          | this case, the created parser is named "<jNull>". `S` in `PS` is for strong name,
  //          | which means the `jNull` parser will not report its internals in an error message. TO
  //          | allow reporting internals, use `P` instead, as shown below with `jArray`, `jObject`,
  //          | and `jObjectEntry`.
  //          |
  //          |         ┌───── as ────┐
  //          |  ┌───  >> ───┐        │
  val jNull = PS { 'n' >>! "ull" as JNull }
  //                    │        │    │
  //                    │        │    └ give it a intuitive name so the error message is
  //                    │        │      easier to understand
  //                    │        │
  //                    │        └ convert the string parser returning "ull" to a parser returning
  //                    │          `JNull`
  //                    │
  //                    └ throw away result from the first parser, which matches 'n', and return
  //                      the result of the second parser, which, in this case, returns "ull". In
  //                      addition, commit right after seeing 'n' to speed up parsing failure in
  //                      case 'n' is followed by things other than "ull". Without committing, the
  //                      parser would keep trying <jBoolean>, <jNumber>, and so on.
  //                
  //                

  //                                               ┌ if matching "true" fails, try the following
  //                                               │ to match false
  //                                               │
  def jBoolean = ('t' >>! "rue" as JBoolean(true)) | 
                 ('f' >>! "alse" as JBoolean(false)) withName "<jBoolean>"
  //                                                 |
  //                                                 └ one could name the parser explicitly like so
  //                                                   without the `P` macro as well

  val jNumber = PS { commitAfter(double.map(JNumber(_))) }
  //                                     │
  //                                     └ similar to `as`, but it consumes the result from the 
  //                                       double parser

  def jString = PS { quoted().map(JString(_)) }

  def jArray : Parser[JValue] = 
    P { '[' >>! (jValue sepBy ',').map(JArray(_)) << commitAfter(']') }
  //                      │
  //                      └ matches `JValue` objects separated by `,` zero or more times and 
  //                        returns the matched `JValue`s inside a `Vector`

  val jObjectKey = PS { whitespaces >> quoted() << whitespaces }

  def jObjectEntry : Parser[(String, JValue)] =
    P { lift(jObjectKey << commitAfter(":"), jValue) }
  //     │
  //     └ combines two parsers `jObjectKey << ":"` and `jValue` and produce a parser that returns a 
  //       tuple containing the parsed key string and `JValue` object.

  def jObject : Parser[JValue] = P {
    '{' >>!
    (jObjectEntry sepBy ',').map(c => JObject(c.toMap))
    << commitAfter('}') 
  }

  def jValue : Parser[JValue] = P {
    whitespaces >> 
    (jNull | jBoolean | jNumber | jString | jArray | jObject) 
    << whitespaces
  }

  @Test
  def `json parser` = testing(jValue) {
    """
    {
      "null" : null,
      "true" : true,
      "false" : false,
      "string" : "foo \"bar\"",
      "array" : [1, "a", null, true, ["nested", "array"], {}],
      "object" : {
        "foo" : "bar"
      }
    }
    """ ~> JObject(Map(
      "null" -> JNull, 
      "true" -> JBoolean(true),
      "false" -> JBoolean(false),
      "string" -> JString("foo \"bar\""), 
      "array" -> JArray(Vector(JNumber(1.0), JString("a"), JNull, JBoolean(true), JArray(Vector(JString("nested"), JString("array"))), JObject(Map()))), 
      "object" -> JObject(Map("foo" -> JString("bar"))), 
      ))

    "blah" ~^ """
      |blah
      |^
      |0: '{'
      |0: <jObject> := '{' >> !(<jObjectEntry> sepBy ',') << '}'!
      |0: <jValue> := <whitespaces> >> (<jNull> | <jBoolean> | <jNumber> | <jString> | <jArray> | <jObject>) << <whitespaces>
    """
    
    """
    {
      "foo": "missing comma on the right"
      "bar": "blah"
    }
    """ ~^ """
      |      "bar": "blah"
      |      ^
      |55: '}'
      |5: <jObject> := '{' >> !(<jObjectEntry> sepBy ',') << '}'!
      |0: <jValue> := <whitespaces> >> (<jNull> | <jBoolean> | <jNumber> | <jString> | <jArray> | <jObject>) << <whitespaces>
    """
  }
}

enum JValue {
  case JNull
  case JBoolean(value: Boolean)
  case JNumber(value: Double)
  case JString(value: String)
  case JArray(value: Vector[JValue])
  case JObject(value: Map[String, JValue])
}     

private def testing[T](parser: string.Parser[T])(block: string.Parser[T] ?=> Unit) = {
  block(using parser)
}
import io.github.tgeng.parse.string.toStringWithInput

private def [T](input: String) ~> (expected: T)(using parser: string.Parser[T]) = {
  parser.parse(input) match {
    case Right(actual) => (actual == expected) match {
      case true => ()
      case false => fail(
        getFailMessagePrefix(parser, input) + "expect output to be\n  " +
        expected.toString.indented(2) + "\nbut actual output is\n  " +
        actual.toString.indented(2) + "\n"
        )
    }
    case Left(e) => {
      val actual = e.toStringWithInput(input)
      fail(
        getFailMessagePrefix(parser, input) + "expect output to be\n  " +
          expected.toString.indented(2) + "\nbut parsing fails with message\n  " +
          actual.indented(2) + "\n"
      )
    }
  }
}

private def [T](input: String) ~^ (errorMessage: String)(using parser: string.Parser[T]) = {
  val t = parser.parse(input)
  val trimmedMessage = errorMessage.trim.asInstanceOf[String].stripMargin
  t match {
     case Right(t) => fail(getFailMessagePrefix(parser, input) +
       s"expect parsing to fail but it succeeds with\n  ${t.toString.indented(2)}\n")
     case Left(e) => {
       val actual = e.toStringWithInput(input)
       actual == trimmedMessage match {
         case false => fail(getFailMessagePrefix(parser, input) +
           s"expect parsing to fail with message\n  ${trimmedMessage.indented(2)}\nbut it fails with message\n  ${actual.indented(2)}\n")
         case _ => ()
       }
     }
  }
}

private def getFailMessagePrefix(parser: ParserT[?, ?], input: Any) =
  s"\nWith ${parser.toString} and given input\n  " +
  input.toString.indented(2) + "\n"

def (s: String | Null) indented(count: Int) =
  if (s == null) null else s.replaceAll("\n", "\n" + (" " * count))