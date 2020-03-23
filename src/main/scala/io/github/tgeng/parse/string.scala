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

  /** Converts a string to a regex parser returning the matched string. */
  def (s: String) rp : Parser[String] = s.rpm.map(_.matched)

  /** Converts a string to a regex parser returning the [[Match]] object. */
  def (s: String) rpm : Parser[Match] = parserMatchingRegex(s.r)

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
}