package io.github.tgeng.parse

import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object string {

  type Parser[T] = ParserT[Char, T]

  private val regexKind = Kind(10, "regex")

  /** Converts a regex to a parser. */
  given parserMatchingRegex as Conversion[Regex, Parser[Match]] = (r: Regex) => new Parser[Match] {
    override def kind : Kind = regexKind
    override def detailImpl = s"/$r/"
    override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char], Match] = {
      r.findPrefixMatchOf(input.content.slice(input.position, input.content.length)) match {
        case Some(regexMatch) => {
          input.position += regexMatch.matched.length
          Right(regexMatch)
        }
        case None => Left(ParserError(input.position, this, null))
      }
    }
  }

  private val stringKind = Kind(10, "string")

  /** Converts a string to a parser that matches the string and returns it. */
  given parserMatchingString as Conversion[String, Parser[String]] = (s: String) => new Parser[String] {
    override def kind : Kind = stringKind
    override def detailImpl = "\"" + s + "\""
    override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char], String] = {
      if (input.content.slice(input.position, input.position + s.length) startsWith s) {
        input.position += s.length
        Right(s)
      } else {
        Left(ParserError(input.position, this, null))
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

  val space : Parser[Char] = charSatisfy(_ == ' ') withStrongName "<space>"
  val spaces : Parser[String] = (space*).map(_.mkString("")) withStrongName "<spaces>"
  val whitespace : Parser[Char] = satisfy(Character.isWhitespace) withStrongName "<whitespace>"
  val whitespaces : Parser[String] = (whitespace*).map(_.mkString("")) withStrongName "<whitespaces>"

  val lf : Parser[Char] = charSatisfy(_ == '\n') withStrongName "<lf>"
  val cr : Parser[Char] = charSatisfy(_ == '\r') withStrongName "<cr>"
  val crlf : Parser[String] = cr >> lf as "\r\n" withStrongName "<crlf>"

  val upper : Parser[Char] = satisfy(Character.isUpperCase) withStrongName "<upper>"
  val lower : Parser[Char] = satisfy(Character.isLowerCase) withStrongName "<lower>"
  val letter : Parser[Char]= satisfy(Character.isLetter) withStrongName "<letter>"
  val digit : Parser[Char] = satisfy(Character.isDigit) withStrongName "<digit>"
  val alphaNum : Parser[Char] = satisfy((c: Char) => Character.isAlphabetic(c.toInt) || Character.isDigit(c)) withStrongName "<alphanum>"

  val int : Parser[Int] = "[-+]?[0-9]+".rp.map(_.toInt) withStrongName "<int>"
  val double : Parser[Double] = "[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)".rp.map(_.toDouble) withStrongName "<double>"

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
    val literal = charSatisfy(c => !allEscapedMapping.values.toSet(c))
    val special = escapeSymbol >> !charSatisfy(allEscapedMapping.keySet).map(allEscapedMapping)

    quoteSymbol >> ((literal|special)*).map(_.mkString("")) << quoteSymbol withStrongName s"<$quoteSymbol-quoted>"
  } 
}