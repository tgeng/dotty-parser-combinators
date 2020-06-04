package io.github.tgeng.parse

import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object string {

  type Parser[T] = ParserT[Char, T]

  private val regexKind = Kind(10, "regex", true)

  /** Converts a regex to a parser. */
  given parserMatchingRegex as Conversion[Regex, Parser[Match]] = (r: Regex) => new Parser[Match] {
    override def kind : Kind = regexKind
    override def detailImpl = s"/$r/"
    override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char] | Null, Match] = {
      r.findPrefixMatchOf(input.content.slice(input.position, input.content.length)) match {
        case Some(regexMatch) => {
          input.position += regexMatch.matched.length
          Right(regexMatch)
        }
        case None => Left(null)
      }
    }
  }

  private val stringKind = Kind(10, "string", true)

  /** Converts a string to a parser that matches the string and returns it. */
  given parserMatchingString as Conversion[String, Parser[String]] = (s: String) => new Parser[String] {
    override def kind : Kind = stringKind
    override def detailImpl = "\"" + s + "\""
    override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char] | Null, String] = {
      if (input.content.slice(input.position, input.position + s.length) startsWith s) {
        input.position += s.length
        Right(s)
      } else {
        Left(null)
      }
    }
  }

  /** Converts a character to a parser that matches and returns it. */
  given parserMatchingChar as Conversion[Char, Parser[Char]] = (c: Char) => satisfy[Char](_ == c) withStrongName s"'$c'"

  /** Converts a string to a regex parser returning the matched string. */
  def (s: String) rp : Parser[String] = s.rpm.map(_.matched)

  /** Converts a string to a regex parser returning the [[Match]] object. */
  def (s: String) rpm : Parser[Match] = parserMatchingRegex(s.r)

  /** Converts a string to a string parser returning the this matched string. */
  def (s: String) p : Parser[String] = s

  /** Converts a char to a char parser returning the this matched char. */
  def (c: Char) p : Parser[Char] = c

  def charSatisfy(predicate: Char => Boolean) : Parser[Char] = satisfy(predicate)

  val space : Parser[Char] = PS { charSatisfy(_ == ' ') }
  val spaces : Parser[String] = PS { (space*).map(_.mkString("")) }
  val whitespace : Parser[Char] = PS { satisfy(Character.isWhitespace) }
  val whitespaces : Parser[String] = PS { (whitespace*).map(_.mkString("")) }

  val lf : Parser[Char] = PS { '\n' }
  val cr : Parser[Char] = PS { '\r' }
  val crlf : Parser[String] = PS { "\r\n" }
  val newline : Parser[Unit] = PS { "\r\n" | "\n" as (()) }
  val blankLine : Parser[Unit] = PS { "[^\\S\n]*\n".rp as (()) }

  val upper : Parser[Char] = PS { satisfy(Character.isUpperCase) }
  val lower : Parser[Char] = PS { satisfy(Character.isLowerCase) }
  val letter : Parser[Char]= PS { satisfy(Character.isLetter) }
  val digit : Parser[Char] = PS { satisfy(Character.isDigit) }
  val alphaNum : Parser[Char] = PS { satisfy((c: Char) => Character.isAlphabetic(c.toInt) || Character.isDigit(c)) }

  val int : Parser[Int] = PS { "[-+]?[0-9]+".rp.map(_.toInt) }
  val double : Parser[Double] = PS { "[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)".rp.map(_.toDouble) }

  def quoted(
      quoteSymbol: Char = '"', 
      escapeSymbol : Char = '\\', 
      additionalEscapeMapping: Map[Char, Char] = Map(
        'n' -> '\n',
        'r' -> '\r',
        't' -> '\t',
        'b' -> '\b',
        'f' -> '\f',
      )
    ) : Parser[String] = {
    val allEscapedMapping = additionalEscapeMapping + (quoteSymbol -> quoteSymbol) + (escapeSymbol -> escapeSymbol)
    val literal = charSatisfy(c => !(allEscapedMapping.values.toSet(c)))
    val special = escapeSymbol >> commitBefore(charSatisfy(allEscapedMapping.keySet).map(allEscapedMapping))

    quoteSymbol >>! ((literal|special)*).map(_.mkString("")) << commitAfter(quoteSymbol) withStrongName s"<$quoteSymbol-quoted>"
  } 
}