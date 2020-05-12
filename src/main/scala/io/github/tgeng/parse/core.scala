package io.github.tgeng.parse

import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer

/** Categorizes parsers for pretty printing.
  * 
  * This information does not affect parsing behavior. It's only intended to 
  * help readability of the string representation of parsers. [[precedence]]
  * determines whether a nested parser should be wrapped inside a pair of
  * parentheses. [[name]] is only for debugging purpose. It's not user visible
  * during normal usage.
  * 
  * The identity of [[Kind]] determines how many levels of nested parsers are
  * printed in the parsing error. For example, a parser like 
  * `p1 | p2 | (p3 & p4)` has 4 levels. If a parsing error happens at `p3` and
  * all levels are printed, we would have
  *
  * * p3
  * * p3 & p4
  * * p2 | (p3 & p4)
  * * p1 | p2 | (p3 & p4)
  *
  * This is redundant so what is actually printed is as follow.
  *
  * * p3
  * * p3 & p4
  * * p1 | p2 | (p3 & p4)
  *
  * That is, because the nested parsers `p1 | (p2 ...)` and `p2 | (p3 ...)` are
  * of the same [[orKind]], they are "squashed" together in the error message.
  */
class Kind(val precedence: Double, val name: String)

/** The [[Kind]] to use when getting the string representation of the outmost
  * parser.
  */
val rootKind = Kind(Double.NegativeInfinity, "root")

/** A generic parser.
  * 
  * @tparam I the parser input type. For example [[Char]] for a parser that
  * parses strings
  * @tparam T the output type of the parser
  */
trait ParserT[-I, +T] {
  def kind : Kind
  /** The short name of the parser. */
  def name(parentKind : Kind = rootKind) : String = detail(parentKind)
  /** The detailed description of the parser. */
  final def detail(parentKind : Kind = rootKind) : String = {
    if (parentKind.precedence > kind.precedence) s"($detailImpl)"
    else detailImpl
  }
  
  /** Subclasses should implements this to provide the detailed description of
    * what this parser does.
    */
  def detailImpl : String

  /** Parses the given input. */
  final def parse(input: IndexedSeq[I]) : Either[ParserError[I], T] = parse(ParserState(input, 0, 0))

  /** Parses the given input. */
  final def parse(input: ParserState[I]) : Either[ParserError[I], T] = {
    val startPosition = input.position
    val result = parseImpl(input)
    result match {
      case Right(t) => Right(t)
      case Left(e) => if (e != null && e.failureParser.kind == kind) {
        // Skip ParserError e since it's from the same kind of parser of this one.
        Left(ParserError(startPosition, this, e.cause))
      } else {
        Left(ParserError(startPosition, this, e))
      }
    }
  }
  /** Subclass should implement this to provide the parsing behavior. */
  protected def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T]

  override def toString() = s"Parser{${name()}}"
}

/** Mutable state of a sequence being parsed. 
  * 
  * @tparam I the type of the input
  */
class ParserState[+I](val content: IndexedSeq[I], var position: Int, var commitPosition: Int)

case class ParserError[-I](
  val position: Int,
  val failureParser: ParserT[I, ?],
  val cause: ParserError[I] | Null) {
  override def toString() : String = {
    val failureParserName = failureParser.name()
    val failureParserDetail = failureParser.detail()
    val detail = if (failureParserName == failureParserDetail) {
      ""
    } else {
      " := " + failureParserDetail
    }
    (if (cause == null) {
      ""
    } else {
      cause.toString() + "\n"
    }) + s"$position: $failureParserName" + detail
  }
}

private def pureKind = Kind(10, "pure")

/** Parser that does not consume any input and simply return the given output.
  *
  * This is the unit of the parser monad. This parser always succeeds.
  */
def pure[I, T](t: T, aName : String = "<pure>") = new ParserT[I, T] {
  override def kind : Kind = pureKind
  override def detailImpl = aName
  override def parseImpl(input: ParserState[I]) = Right(t)
}

/** Converts the given parser to another parser that applies [[f]] to the
  * result∷
  *
  * This is the standard `map` function that witnesses the functoriality of 
  * [[ParserT]].
  */
def [I, T, R](p: ParserT[I, T]) map(f: T => R): ParserT[I, R] = {
  new ParserT[I, R] {
    override def kind : Kind = p.kind
    override def name(k: Kind) = p.name(k)
    override def detailImpl = p.detailImpl
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, R] =
      p.parse(input).map(f)
  }
}

private val flatMapKind = Kind(5, "flatMap")

/** Converts the given parser to another parser that depends on the result of
  * the input parser∷
  *
  * This is the standard `flatMap` or `join` function for the parser monad.
  */
def [I, T, R](p: ParserT[I, T]) flatMap(f: T => ParserT[I, R]) : ParserT[I, R] = {
  var nextParser : ParserT[I, R] | Null = null
  new ParserT[I, R] {
    override def kind : Kind = flatMapKind
    override def detailImpl = {
      val np = nextParser
      p.name(kind) + " " + (if (np == null) "<?>" else np.name(kind))
    }
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, R] =
      p.parse(input).flatMap(t => {
          val p = f(t)
          nextParser = p
          p.parse(input)
        })
  }
}

/** Gives the input parser a name for pretty printing.
  * 
  * In addition, the input parser is used lazily such that the input parser can
  * reference itself directly or transitively. Any recursive parsers must have a
  * name, otherwise building the parser itself will incur infinite recursions,
  * causing stack overflow while building the parser.
  */
def [I, T](p: => ParserT[I, T]) withName(newName: String) : ParserT[I, T] = 
  p.withNameAndDetail(newName, null)

/** Gives the input parser a name for pretty printing.
  * 
  * In addition, the input parser is used lazily such that the input parser can
  * reference itself directly or transitively. Any recursive parsers must have a
  * name, otherwise building the parser itself will incur infinite recursions,
  * causing stack overflow while building the parser.
  *
  * Comparing with [[withName]], this method suppresses the [[detail]] of the
  * input parser.
  */
def [I, T](p: => ParserT[I, T]) withStrongName(newName: String) : ParserT[I, T] =
   p.withNameAndDetail(newName, newName)

private def [I, T](p: => ParserT[I, T]) withNameAndDetail(newName: String, newDetail: String|Null) : ParserT[I, T] = new ParserT[I, T] {
  override def kind : Kind = p.kind
  override def name(parentKind: Kind): String = newName
  override def detailImpl = if(newDetail == null) p.detailImpl else newDetail
  override def parseImpl(input: ParserState[I]) = p.parse(input)
}

/** Gives the input parser a different detail message and kind. */
def [I, T](p: ParserT[I, T]) withDetailAndKind(newDetail: String, newKind: Kind) : ParserT[I, T] = 
  p.withDetailFnAndKind(_ => newDetail, newKind)

/** Gives the input parser a different detail message and kind.
  * 
  * Comparing with [[withDetailAndKind]], this method allows the detail
  * description to depend on the detail of the input parser.
  */
def [I, T](p: ParserT[I, T]) withDetailFnAndKind(detailTransformer: String => String, newKind : Kind) : ParserT[I, T] = new ParserT[I, T] {
  override def kind : Kind = newKind
  override def detailImpl = detailTransformer(p.detailImpl)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T] = p.parse(input) match {
    case Left(ParserError(position, failureParser, cause)) => Left(cause)
    case t@_ => t
  }
}

private val encapsulatedKind = Kind(10, "encapsulated")

/** Encapsulates the input parser so that the effect of committing won't leak
  * to wrapping parsers.
  */
def encapsulated[I, T](p: ParserT[I, T]) = new ParserT[I, T] {
  override def kind : Kind = encapsulatedKind
  override def detailImpl = s"{ ${p.name()} }"
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], T] = {
    val commitPosition = input.commitPosition
    val result = p.parse(input);
    input.commitPosition = commitPosition
    result
  }
}

private val commitToKind = Kind(10, "!")

/** Commit to the right of this parser. See doc of [[|]] for details. */
def [I, T](p: ParserT[I, T])unary_! = new ParserT[I, T] {
  override def kind : Kind = commitToKind
  override def detailImpl = "!" + p.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], T] = {
    input.commitPosition = input.position
    p.parse(input) match {
        case Left(ParserError(position, failureParser, cause)) => Left(ParserError(position, this, cause))
        case t@_ => t
    }
  }
}

/** Commit to the left of this parser. See doc of [[|]] for details. */
def [I, T](p: ParserT[I, T])! = new ParserT[I, T] {
  override def kind : Kind = commitToKind
  override def detailImpl = p.name(kind) + "!"
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], T] = {
    p.parse(input) match {
        case Left(ParserError(position, failureParser, cause)) => Left(ParserError(position, this, cause))
        case t@_ => {
          input.commitPosition = input.position
          t
        }
    }
  }
}

private val notKind = Kind(5, "not")

/** Negates the input parser
  * 
  * If the input parser matches, the negated parser would fail. Otherwise, the
  * negated parser succeeds with [[Unit]]. This combinator is often useful with
  * [[&]]. For example `anyToken & not("def" << not(letter))` would match any 
  * tokens other than the keyword "def".
  */
def not[I](p: ParserT[I, ?]) = new ParserT[I, Unit] {
  override def kind : Kind = notKind
  override def detailImpl = "not " + p.name(kind)
  override def parseImpl(input: ParserState[I]) = {
    val position = input.position
    val commitPosition = input.commitPosition
    val result = p.parse(input) match {
      case Right(_) => Left(ParserError(position, this, null))
      case Left(_) => Right(())
    }
    input.position = position
    input.commitPosition = commitPosition
    result
  }
}

private val orKind = Kind(1, "|")

/** Or combinator.
  * 
  * Combines the input parsers into one parser that tries each input parser 
  * until one succeeds, at which point it will stop trying the next one.
  * This is probably the most important combinator for a parser combinator
  * library.
  *
  * By default, with or combinator, the resulted parser keeps trying the
  * alternative until one succeeds or all of them fail. But if the input parser
  * commits to some parsing position and fails. All next tries will be skipped,
  * causing the resulted parser to fail directly, potentially significantly
  * speed up parsing. This is contrary to the Haskell parser library "parsec", 
  * which by default always commits.
  * 
  * To commit parsed contents, use [[!]] or [[unary_!]] combinators.
  */
def [I, T](p1: ParserT[I, T]) | (p2: ParserT[I, T]) : ParserT[I, T] = new ParserT[I, T] {
  override def kind : Kind = orKind
  override def detailImpl = p1.name(kind) + " | " + p2.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T] = {
    val startPosition = input.position
    p1.parse(input) match {
      case Left(_) if (startPosition >= input.commitPosition) => {
        input.position = startPosition
        p2.parse(input)
      }
      case t1@_ => t1
    }
  }
}

private val andKind = Kind(2, "&")

/** And combinator.
  *
  * Combines input parsers and pass on the result of the left parser. The result
  * of the right parser is discarded. But if the right parser fails, the overall
  * parser fails as well.
  */ 
def [I, T](p: ParserT[I, T]) & (cond: ParserT[I, Any]): ParserT[I, T] = new ParserT[I, T] {
  override def kind : Kind = andKind
  override def detailImpl = p.name(kind) + " & " + cond.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T] = {
    val startPosition = input.position
    val startCommitPosition = input.commitPosition
    p.parse(input) match {
      case t@Right(_) => {
        val finishPosition = input.position
        val commitPosition = input.commitPosition
        input.position = startPosition
        input.commitPosition = startCommitPosition
        val result = cond.parse(input) match {
          case Right(_) => t
          case Left(e) => Left(e)
        }
        input.position = finishPosition
        input.commitPosition = commitPosition
        result
      }
      case e@_ => e
    }
  }
}

/** Repeats the parser zero or more times and return all matches in a [[Vector]]. */
def [I, T](p: ParserT[I, T])* = new ParserT[I, Vector[T]](){
  private val starKind = Kind(9, "*")
  override def kind : Kind = starKind
  override def detailImpl = p.name(kind) + "*"
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], Vector[T]] = {
    val result = ArrayBuffer[T]()
    while(true) {
      val startPosition = input.position
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) if (startPosition >= input.commitPosition) => {
          input.position = startPosition
          return Right(result.toVector)
        }
        case Left(e) => return Left(e)
      }
    }
    throw AssertionError("impossible branch")
  }
}

/** Repeats the parser exactly [[count]] number of times and return all matches
  * in a [[Vector]].
  */
def [I, T](count: Int) *(p: ParserT[I, T]) = new ParserT[I, Vector[T]] {
  private val repeatKind = Kind(8, "n*_")
  override def kind : Kind = repeatKind
  override def detailImpl = s"$count * " + p.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], Vector[T]] = {
    val position = input.position;
    val result = new ArrayBuffer[T](count)
    var i = 0
    while (i < count) {
      i += 1
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) => return Left(ParserError(position, this, e))
      }
    }
    Right(result.toVector)
  }
}

private def positionKind = Kind(10, "position")

/** Special parser that returns the current parsing position. */
val position = new ParserT[Any, Int] {
  override def kind : Kind = positionKind
  override def detailImpl = "<pos>"
  override def parseImpl(input: ParserState[Any]) = Right(input.position)
}

private val predicateKind = Kind(10, "satisfy")

/** Simple parser that succeeds if the current parser input matches the given 
  * predicate.
  */
def satisfy[I](predicate: I => Boolean) = new ParserT[I, I] {
  override def kind : Kind = predicateKind
  override def detailImpl = "<satisfy>"
  override def parseImpl(input: ParserState[I]) = {
    val position = input.position
    if (position >= input.content.size) {
      return Left(ParserError(position, this, null))
    }
    val t = input.content(position)
    input.position += 1
    if (predicate(t)) {
      Right(t)
    } else {
      Left(ParserError(position, this, null))
    }
  }
}

/** Augment the given parser with additional predicate testing the parsed 
 * result. If the result fails the predicate, the returned parser fails parsing.
 */
def [I, T](p: ParserT[I, T]) satisfying(predicate: T => Boolean, predicateName: String = "some custom predicate") = new ParserT[I, T] {
    override def kind : Kind = p.kind
    override def detailImpl = p.detailImpl + " satisfying " + predicateName
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T] =
      val position = input.position
      p.parse(input).flatMap{ t =>
        predicate(t) match {
          case true => Right(t)
          case false => Left(ParserError(position, this, null))
        }
      }
}

/** Facilitates implicit conversion when needed. */
inline def parser[I, T](p: ParserT[I, T]) = p

/** Facilitates implicit conversion. In addition name the parser with the
  * enclosing definition automatically. For example
  *  
  * {{{
  * scala> val keyword = P{ "def" | "val" }
  * val keyword: io.github.tgeng.parse.ParserT[Char, String] = Parser{keyword}
  * }}}
  * 
  */
inline def P[I, T](inline parser: => ParserT[I, T]) : ParserT[I, T] = 
  parser.withNameAndDetail("<" + enclosingName(parser) + ">", null)


private inline def enclosingName(inline e: Any): String = ${
  enclosingNameImpl('e)
}

import scala.quoted._

private def enclosingNameImpl(e: Expr[Any])(using qctx: QuoteContext): Expr[String] = {
  import qctx.tasty._
  val name = rootContext.owner.owner.name
  //                                ^
  //                                the macro itself, which we will skip
  Literal(Constant(name)).seal.asInstanceOf[Expr[String]]
}