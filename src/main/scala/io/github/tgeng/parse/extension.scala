package io.github.tgeng.parse

import scala.collection.IndexedSeq

/** Converts the result of this parser to [[r]]. */
def [I, R](p: ParserT[I, ?]) as(r: R) : ParserT[I, R] = p.map(_ => r)

/** Does nothing and always succeeds. */
val nothing: ParserT[Any, Unit] = pure(()) withStrongName "<nothing>"

/** Matches any input token and returns it. */
def any[I] : ParserT[I, I] = satisfy[I](_ => true) withStrongName "<any>"

/** Matches the end of the input. */
val eof : ParserT[Any, Unit] = not(any) withStrongName "<eof>"

/** Skips the current token. */
val skip : ParserT[Any, Unit] = satisfy[Any](_ => true).as(()) withStrongName "<skip>"

/** Matches any of the given [[candidates]]. */
def anyOf[I](candidates : Seq[I]) : ParserT[I, I] = 
  satisfy[I](candidates.contains(_)) withStrongName s"<anyOf{${candidates.mkString(", ")}}>"

/** Matches any of the given [[candidates]]. */
def anyOf[I](candidates : Set[I]) : ParserT[I, I] = 
  satisfy[I](candidates.contains(_)) withStrongName s"<anyOf{${candidates.mkString(", ")}}>"

val prefixSuffixKind = Kind(4, "prefixSuffix")

/** Matches [[p1]] and [[p2]] and outputs result of [[p2]]. */
def [I, T](p1: ParserT[I, ?]) >> (p2: ParserT[I, T]) : ParserT[I, T] = (for {
  _ <- p1
  t <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " >> " + p2.name(prefixSuffixKind),
  prefixSuffixKind)

/** Matches [[p1]] and [[p2]] and outputs result of [[p1]]. */
def [I, T](p1: ParserT[I, T]) << (p2: ParserT[I, ?]) : ParserT[I, T] = (for {
  t <- p1
  _ <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " << " + p2.name(prefixSuffixKind),
  prefixSuffixKind)

/** Repeats the parser one or more times and return all matches in a [[Vector]]. */
def [I, T](p: ParserT[I, T])+ : ParserT[I, Vector[T]] = {
  val kind = Kind(9, "+")
  (for {
  t <- p
  ts <- p*
} yield t +: ts) withDetailAndKind(p.name(kind) + "+", kind)
}

/** Repeats the parser zero or one times and return an option of the result. */
def [I, T](p: ParserT[I, T])? : ParserT[I, Option[T]] = {
  val kind = Kind(9, "?")
  (p.map(Some[T]) | (nothing as None)) withDetailAndKind(
    p.name(kind) + "?",
    kind
  )
}

/** Parses [[p]] repeatedly at once, separated by [[s]]. */
def [I, T](p: ParserT[I, T]) sepBy1 (s: ParserT[I, ?]) : ParserT[I, Vector[T]] = {
  val sepKind = Kind(0, "sepBy1")
  p +: ((s >> p)*) withDetailAndKind (
  s"${p.name(sepKind)} sepBy1 ${s.name(sepKind)}",
  sepKind)
}

/** Parses [[p]] repeatedly at zero or more times, separated by [[s]]. */
def [I, T](p: ParserT[I, T]) sepBy (s: ParserT[I, ?]) : ParserT[I, Vector[T]] = {
  val sepKind = Kind(0, "sepBy")
  (p.sepBy1(s) | (nothing as Vector.empty)) withDetailAndKind (
  s"${p.name(sepKind)} sepBy ${s.name(sepKind)}",
  sepKind)
}

/** Parses [[p]] repeatedly at exactly [[count]] number of times, separated by
  * [[s]].
  */
def [I, T](p: ParserT[I, T]) sepByN (count: Int) (s: ParserT[I, ?]) : ParserT[I, Vector[T]] = {
  val sepKind = Kind(0, "sepByN")
  count match {
    case 0 => nothing as Vector.empty[T]
    case 1 => p.map(Vector[T](_))
    case n => p +: (n - 1) * (s >> p)
  } withDetailAndKind (
    s"${p.name(sepKind)} sepByN($count) ${s.name(sepKind)}",
    sepKind)
}

/** Chains [[elemParsers]] with [[opParsers]] and output the result applying
  * resulted function from [[opParsers]] with results from [[elemParsers]].
  * Function application starts from results on the left.
  */
def [I, T](elemParser: ParserT[I, T]) chainedLeftBy(opParser: ParserT[I, (T, T) => T]) : ParserT[I, T] = {
  val chainKind = Kind(0, "chainedLeftBy")
  foldLeft(elemParser, opParser, elemParser)
  .withDetailAndKind(
    s"${elemParser.name(chainKind)} chainedLeftBy ${opParser.name(chainKind)}",
    chainKind)
}

/** Chains [[elemParsers]] with [[opParsers]] and output the result applying
  * resulted function from [[opParsers]] with results from [[elemParsers]].
  * Function application starts from results on the right.
  */
def [I, T](elemParser: ParserT[I, T]) chainedRightBy(opParser: ParserT[I, (T, T) => T]) : ParserT[I, T] = {
  val chainKind = Kind(0, "chainedLeftBy")
  foldRight(elemParser, opParser, elemParser)
  .withDetailAndKind(
  s"${elemParser.name(chainKind)} chainedRightBy ${opParser.name(chainKind)}",
  chainKind)
}

/** Folds results from the given parsers. Just like the standard fold-left operation. */
def foldLeft[I, L, R](accParser: ParserT[I, L], opParser: ParserT[I, (L, R) => L], elemParser: ParserT[I, R]) : ParserT[I, L] = {
  val foldKind = Kind(10, "foldLeft")
  (for {
    first <- accParser
    rest <- (for {
      op <- opParser
      elem <- elemParser
    } yield (op, elem))*
  } yield rest.foldLeft(first)((acc, p) => p._1(acc, p._2))).withDetailAndKind(
    s"foldLeft{${accParser.name(foldKind)} (${opParser.name(foldKind)} ${elemParser.name(foldKind)})*}",
    foldKind)
}

/** Folds results from the given parsers. Just like the standard fold-right operation. */
def foldRight[I, L, R](elemParser: ParserT[I, L], opParser: ParserT[I, (L, R) => R], accParser: ParserT[I, R]) : ParserT[I, R] = {
  val foldKind = Kind(10, "foldRight")
  (for {
    front <- ((for {
      elem <- elemParser
      op <- opParser
    } yield (op, elem))*)
    last <- accParser
  } yield front.foldRight(last)((p, acc) => p._1(p._2, acc))).withDetailAndKind(
    s"foldRight{(${elemParser.name(foldKind)} ${opParser.name(foldKind)})* ${accParser.name(foldKind)}}",
    foldKind)
}

def applyKind = Kind(10, "apply")

/** Applies the function resulted from [[fP]] with the argument resulted from 
  * [[arg1P]]. 
  */
def [I, F, T](fnP: ParserT[I, F => T])<*>(
  arg1P: ParserT[I, F]
  ) : ParserT[I, T] = (for {
  fn <- fnP
  arg1 <- arg1P
} yield fn(arg1)) withDetailAndKind(
  s"${fnP.name(applyKind)} <*> (${arg1P.name()})",
  applyKind)

/** Applies the function resulted from [[fP]] with the argument resulted from 
  * [[arg1P]], [[arg2P]].
  */
def [I, F1, F2, T](fnP: ParserT[I, (F1, F2) => T])<*>(
  arg1P: ParserT[I, F1],
  arg2P: ParserT[I, F2],
  ) : ParserT[I, T] = (for {
  fn <- fnP
  arg1 <- arg1P
  arg2 <- arg2P
} yield fn(arg1, arg2)) withDetailAndKind (
  s"${fnP.name(applyKind)} <*> (${arg1P.name()}, ${arg2P.name()})",
  applyKind)

/** Applies the function resulted from [[fP]] with the argument resulted from 
  * [[arg1P]], [[arg2P]], [[arg3P]].
  */
def [I, F1, F2, F3, T](fnP: ParserT[I, (F1, F2, F3) => T])<*>(
  arg1P: ParserT[I, F1],
  arg2P: ParserT[I, F2],
  arg3P: ParserT[I, F3],
  ) : ParserT[I, T] = (for {
  fn <- fnP
  arg1 <- arg1P
  arg2 <- arg2P
  arg3 <- arg3P
} yield fn(arg1, arg2, arg3)) withDetailAndKind (
  s"${fnP.name(applyKind)} <*> (${arg1P.name()}, ${arg2P.name()}, ${arg3P.name()})",
  applyKind)

/** Applies the function resulted from [[fP]] with the argument resulted from
  * [[arg1P]], [[arg2P]], [[arg3P]], [[arg4P]].
  */
def [I, F1, F2, F3, F4, T](fnP: ParserT[I, (F1, F2, F3, F4) => T])<*>(
  arg1P: ParserT[I, F1],
  arg2P: ParserT[I, F2],
  arg3P: ParserT[I, F3],
  arg4P: ParserT[I, F4],
  ) : ParserT[I, T] = (for {
  fn <- fnP
  arg1 <- arg1P
  arg2 <- arg2P
  arg3 <- arg3P
  arg4 <- arg4P
} yield fn(arg1, arg2, arg3, arg4)) withDetailAndKind (
  s"${fnP.name(applyKind)} <*> (${arg1P.name()}, ${arg2P.name()}, ${arg3P.name()}, ${arg4P.name()})",
  applyKind)

val prependAppendConcat = Kind(7, "prependAppendConcat")

/** Combine the result of two parsers and put them into a [[Vector]]. */
def [I, T](p1: ParserT[I, T]) +:+ (p2: ParserT[I, T]) : ParserT[I, Vector[T]] = (for {
  t1 <- p1
  t2 <- p2
} yield Vector(t1, t2)).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} +:+ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

/** Prepend result from [[p1]] to result of [[p2]]. */
def [I, T, CC[_], C <: scala.collection.SeqOps[T, CC, C]](p1: ParserT[I, T]) +: (p2: ParserT[I, C]) : ParserT[I, CC[T]] = (for {
  t <- p1
  ts <- p2
} yield t +: ts).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} +: ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

/** Append result from [[p2]] to result of [[p1]]. */
def [I, T, CC[_], C <: scala.collection.SeqOps[T, CC, C]](p1: ParserT[I, C]) :+ (p2: ParserT[I, T]) : ParserT[I, CC[T]] = (for {
  ts <- p1
  t <- p2
} yield ts :+ t).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} :+ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

/** Concatenates result from [[p1]] with result of [[p2]]. */
def [I, T, CC[_], C <: scala.collection.IterableOps[T, CC, C]](p1: ParserT[I, C]) ++ (p2: ParserT[I, C]) : ParserT[I, CC[T]] = (for {
  ts1 <- p1
  ts2 <- p2
} yield ts1 ++ ts2).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} ++ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

def  lift[I, T, CC[_], C <: scala.collection.SeqOps[ParserT[I, T], CC, C]](parsers: C) : ParserT[I, CC[T]] =
  parsers.foldLeft(
      // I could not figure out how to create an empty CC[T] without using this
      // casting.
      pure[I, CC[T]](parsers.empty.asInstanceOf[CC[T]])
    )(
      // Sadly CC[T] is too weak here to call `:+` defined above. So we force
      // cast and cast back.
      (acc, e) => (acc.asInstanceOf[ParserT[I, Seq[T]]] :+ e).asInstanceOf[ParserT[I, CC[T]]]) withStrongName 
        s"lift{${parsers.toSeq.map(_.name()).mkString(", ")}}"

def lift[I, A, B](tuple: (ParserT[I, A], ParserT[I, B])) : ParserT[I, (A, B)] = (for {
  a <- tuple._1
  b <- tuple._2
} yield (a, b)) withStrongName s"(${tuple._1.name()}, ${tuple._2.name()})"

def lift[I, A, B, C](tuple: (ParserT[I, A], ParserT[I, B], ParserT[I, C])) : ParserT[I, (A, B, C)] = (for {
  a <- tuple._1
  b <- tuple._2
  c <- tuple._3
} yield (a, b, c)) withStrongName s"(${tuple._1.name()}, ${tuple._2.name()}, ${tuple._3.name()})"

def lift[I, A, B, C, D](tuple: (ParserT[I, A], ParserT[I, B], ParserT[I, C], ParserT[I, D])) : ParserT[I, (A, B, C, D)] = (for {
  a <- tuple._1
  b <- tuple._2
  c <- tuple._3
  d <- tuple._4
} yield (a, b, c, d)) withStrongName s"(${tuple._1.name()}, ${tuple._2.name()}, ${tuple._3.name()}, ${tuple._4.name()})"
