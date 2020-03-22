package io.github.tgeng.parse

import scala.language.implicitConversions
import scala.util.matching.Regex

object string {

  type Parser[T] = ParserT[Char, T]

  private val regexKind = Kind(10, "regex")

  given parserMatchingRegex as Conversion[Regex, Parser[String]] = (r: Regex) => new Parser[String] {
    override def kind : Kind = regexKind
    override def detailImpl = s"/$r/"
    override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char] | Null, String] = {
      r.findPrefixOf(input.content.slice(input.position, input.content.length)) match {
        case Some(matched) => {
          input.position += matched.length
          Right(matched)
        }
        case None => Left(ParserError(input.position, this, null))
      }
    }
  }

  private val stringKind = Kind(10, "string")

  given parserMatchingString as Conversion[String, Parser[String]] = (s: String) => new Parser[String] {
    override def kind : Kind = stringKind
    override def detailImpl = "\"" + s + "\""
    override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char] | Null, String] = {
      if (input.content.slice(input.position, input.position + s.length) startsWith s) {
        input.position += s.length
        Right(s)
      } else {
        Left(ParserError(input.position, this, null))
      }
    }
  }

  given parserMatchingChar as Conversion[Char, Parser[Char]] = (c: Char) => satisfy[Char](_ == c) withStrongName s"'$c'"
}