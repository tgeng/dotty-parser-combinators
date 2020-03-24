# Dotty Parser Combinators

This is a parser combinator library for [Dotty](https://dotty.epfl.ch/).
Since Dotty is still under active development, the language features and APIs
are still unstable. As a result, this library offers no guarantees on
backward compatibility for each minor releases. In addition, the library
would try to catch up with each release of Dotty.

This project focuses on _simplicity_ over performance. It has a flexible API
and is powerful enough for typical use cases in implementing a programming
language. It also has good support for error reporting.

It's not production ready yet. I created this library since I haven't found a
parser library that works with Dotty. I use this for my personal projects and
would try my best to fix bugs and improve performance along the way. Anyone
is welcome to contribute, either by raising issues or sending PRs. I will try
to address them as soon as possible.

The library takes inspiration from
[parsec](https://hackage.haskell.org/package/parsec) and
[Lightyear](https://github.com/ziman/lightyear).

## Get Started

TODO: add this section after this is published.

## Usage

Import the following where this library is needed.

```scala
import io.github.tgeng.parse._
import io.github.tgeng.parse.string.{_, given _}
```

- `io.github.tgeng.parse`: the core `ParserT[I, T]` trait and various generic
  combinators.

- `io.github.tgeng.parse.string`: string parser trait `Parser[T]`, which is a
  type alias to `ParserT[Char, T], and other combinators that
  are specific to parsing strings.

### Basics

For the sake of simplicity, assuming we are parsing strings. Essentially we
want to use this library to build a `Parser[T]` object that parses a string and
output a result of type `T`. For example,

```scala
scala> val positiveInt = "[0-9]+".rp.map(_.toInt)
val positiveInt: io.github.tgeng.parse.ParserT[Char, Int] = Parser{/[0-9]+/}

scala> positiveInt.parse("123")
val res0: Either[io.github.tgeng.parse.ParserError[Char], Int] = Right(123)
```

### Example

```
```

### Matching strings

Scala `Char`, `String`, and `Regex` can be implicitly converted to parsers
that matches things intuitively. In addition to implicit conversion,
extension methods `rp` (**r**egex **p**arser) and `rpm` (**r**egex **p**arser
outputing **M**atch object) are defined on `String`s to convert the string to
a regex parser that output the matched `String` and
`scala.util.matching.Regex.Match`, respectively.

```scala
scala> import scala.language.implicitConversions

scala> val p1 : Parser[String] = "abc"
val p1: io.github.tgeng.parse.string.Parser[String] = Parser{"abc"}

scala> p1.parse("abc")
val res1: Either[io.github.tgeng.parse.ParserError[Char], String] = Right(abc)

scala> p1.parse("def")
val res2: Either[io.github.tgeng.parse.ParserError[Char], String] = Left(0: "abc")

scala> val p2 : Parser[String] = "[0-9]".rp
val p2: io.github.tgeng.parse.string.Parser[String] = Parser{/[0-9]/}

scala> p2.parse("123")
val res3: Either[io.github.tgeng.parse.ParserError[Char], String] = Right(1)

scala> p2.parse("abc")
val res4: Either[io.github.tgeng.parse.ParserError[Char], String] = Left(0: /[0-9]/)

scala> val p3 : Parser[Char] = 'x'
val p3: io.github.tgeng.parse.string.Parser[Char] = Parser{'x'}

scala> p3.parse("x")
val res5: Either[io.github.tgeng.parse.ParserError[Char], Char] = Right(x)

scala> p3.parse("y")
val res6: Either[io.github.tgeng.parse.ParserError[Char], Char] = Left(0: 'x')
```

### Combinators

Many common combinators are provided. To see all of them please refer to
[core.scala](https://github.com/tgeng/dotty-parser-combinators/blob/master/src/main/scala/io/github/tgeng/parse/core.scala)
and
[extension.scala](https://github.com/tgeng/dotty-parser-combinators/blob/master/src/main/scala/io/github/tgeng/parse/extension.scala).

For more examples, please refer to
[`ParserTest.scala`](https://github.com/tgeng/dotty-parser-combinators/blob/master/src/test/scala/io/github/tgeng/parse/ParserTest.scala).

## Versions

| Dotty    | dotty-parser-combinators |
| -------- | ------------------------ |
| 0.23-RC1 | 0.1.0                    |
