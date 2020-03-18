package io.github.tgeng.parse

def [I, R](p: ParserT[I, ?]) as(r: R) : ParserT[I, R] = p.map(_ => r)

val empty: ParserT[Any, Unit] = pure(()) withStrongName "<empty>"
def any[I] : ParserT[I, I] = satisfy[I](_ => true) withStrongName "<any>"
val eof : ParserT[Any, Unit] = not(any) withStrongName "<eof>"
val skip : ParserT[Any, Unit] = satisfy[Any](_ => true).as(()) withStrongName "<skip>"
def anyOf[I](candidates : Seq[I]) : ParserT[I, I] = satisfy[I](candidates.contains(_)) withStrongName s"<anyOf{${candidates.mkString(", ")}}>"

val prefixSuffixKind = Kind(4, "prefixSuffix")

def [I, T](p1: ParserT[I, ?]) >> (p2: ParserT[I, T]) : ParserT[I, T] = (for {
  _ <- p1
  t <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " >> " + p2.name(prefixSuffixKind),
  prefixSuffixKind)

def [I, T](p1: ParserT[I, T]) << (p2: ParserT[I, ?]) : ParserT[I, T] = (for {
  t <- p1
  _ <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " << " + p2.name(prefixSuffixKind),
  prefixSuffixKind)

def [I, T](p: ParserT[I, T])+ : ParserT[I, IndexedSeq[T]] = {
  val kind = Kind(9, "+")
  (for {
  t <- p
  ts <- p*
} yield t +: ts) withDetailAndKind(p.name(kind) + "+", kind)
}

def [I, T](p: ParserT[I, T])? : ParserT[I, Option[T]] = {
  val kind = Kind(0, "?")
  (p.map(Some[T]) | (empty as None)) withDetailAndKind(
    p.name(kind) + "?",
    kind
  )
}

def [I, T](p: ParserT[I, T]) sepBy1 (s: ParserT[I, ?]) : ParserT[I, IndexedSeq[T]] = {
  val sepKind = Kind(0, "sepBy1")
  p +: ((s >> p)*) withDetailAndKind (
  s"${p.name(sepKind)} sepBy1 ${s.name(sepKind)}",
  sepKind)
}

def [I, T](p: ParserT[I, T]) sepBy (s: ParserT[I, ?]) : ParserT[I, IndexedSeq[T]] = {
  val sepKind = Kind(0, "sepBy")
  (p.sepBy1(s) | (empty as IndexedSeq.empty)) withDetailAndKind (
  s"${p.name(sepKind)} sepBy ${s.name(sepKind)}",
  sepKind)
}

def [I, T](p: ParserT[I, T]) sepByN (count: Int) (s: ParserT[I, ?]) : ParserT[I, IndexedSeq[T]] = {
  val sepKind = Kind(0, "sepByN")
  count match {
    case 0 => empty as IndexedSeq.empty[T]
    case 1 => p.map(IndexedSeq[T](_))
    case n => p +: (n - 1) * (s >> p)
  } withDetailAndKind (
    s"${p.name(sepKind)} sepByN($count) ${s.name(sepKind)}",
    sepKind)
}

def [I, T](elemParser: ParserT[I, T]) chainedLeftBy(opParser: ParserT[I, (T, T) => T]) : ParserT[I, T] = {
  val chainKind = Kind(0, "chainedLeftBy")
  foldLeft(elemParser, opParser, elemParser)
  .withDetailAndKind(
    s"${elemParser.name(chainKind)} chainedLeftBy ${opParser.name(chainKind)}",
    chainKind)
}

def [I, T](elemParser: ParserT[I, T]) chainedRightBy(opParser: ParserT[I, (T, T) => T]) : ParserT[I, T] = {
  val chainKind = Kind(0, "chainedLeftBy")
  foldRight(elemParser, opParser, elemParser)
  .withDetailAndKind(
  s"${elemParser.name(chainKind)} chainedRightBy ${opParser.name(chainKind)}",
  chainKind)
}

def foldLeft[I, L, R](leftMostParser: ParserT[I, L], opParser: ParserT[I, (L, R) => L], elemParser: ParserT[I, R]) : ParserT[I, L] = {
  val foldKind = Kind(10, "foldLeft")
  (for {
    first <- leftMostParser
    rest <- (for {
      op <- opParser
      elem <- elemParser
    } yield (op, elem))*
  } yield rest.foldLeft(first)((acc, p) => p._1(acc, p._2))).withDetailAndKind(
    s"foldLeft{${leftMostParser.name(foldKind)} (${opParser.name(foldKind)} ${elemParser.name(foldKind)})*}",
    foldKind)
}

def foldRight[I, L, R](elemParser: ParserT[I, L], opParser: ParserT[I, (L, R) => R], rightMostParser: ParserT[I, R]) : ParserT[I, R] = {
  val foldKind = Kind(10, "foldRight")
  (for {
    front <- ((for {
      elem <- elemParser
      op <- opParser
    } yield (op, elem))*)
    last <- rightMostParser
  } yield front.foldRight(last)((p, acc) => p._1(p._2, acc))).withDetailAndKind(
    s"foldRight{(${elemParser.name(foldKind)} ${opParser.name(foldKind)})* ${rightMostParser.name(foldKind)}}",
    foldKind)
}

def applyKind = Kind(10, "apply")

def [I, F, T](fnP: ParserT[I, F => T]) $(
  fP: => ParserT[I, F]
  ) : ParserT[I, T] = (for {
  fn <- fnP
  f <- fP
} yield fn(f)) withDetailAndKind(
  s"${fnP.name(applyKind)} $$ (${fP.name()})",
  applyKind)

def [I, F1, F2, T](fnP: ParserT[I, (F1, F2) => T]) $ (
  f1P: => ParserT[I, F1],
  f2P: => ParserT[I, F2],
  ) : ParserT[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
} yield fn(f1, f2)) withDetailAndKind (
  s"${fnP.name(applyKind)} $$ (${f1P.name()}, ${f2P.name()})",
  applyKind)

def [I, F1, F2, F3, T](fnP: ParserT[I, (F1, F2, F3) => T]) $ (
  f1P: => ParserT[I, F1],
  f2P: => ParserT[I, F2],
  f3P: => ParserT[I, F3],
  ) : ParserT[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
} yield fn(f1, f2, f3)) withDetailAndKind (
  s"${fnP.name(applyKind)} $$ (${f1P.name()}, ${f2P.name()}, ${f3P.name()})",
  applyKind)

def [I, F1, F2, F3, F4, T](fnP: ParserT[I, (F1, F2, F3, F4) => T]) $ (
  f1P: => ParserT[I, F1],
  f2P: => ParserT[I, F2],
  f3P: => ParserT[I, F3],
  f4P: => ParserT[I, F4],
  ) : ParserT[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
  f4 <- f4P
} yield fn(f1, f2, f3, f4)) withDetailAndKind (
  s"${fnP.name(applyKind)} $$ (${f1P.name()}, ${f2P.name()}, ${f3P.name()}, ${f4P.name()})",
  applyKind)

val prependAppendConcat = Kind(7, "prependAppendConcat")

def [I, T](p1: ParserT[I, T]) +:+ (p2: ParserT[I, T]) : ParserT[I, IndexedSeq[T]] = (for {
  t1 <- p1
  t2 <- p2
} yield IndexedSeq(t1, t2)).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} +:+ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

def [I, T, S <: IndexedSeq[T]](p1: ParserT[I, T]) +: (p2: ParserT[I, IndexedSeq[T]]) : ParserT[I, IndexedSeq[T]] = (for {
  t <- p1
  ts <- p2
} yield t +: ts).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} +: ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

def [I, T](p1: ParserT[I, IndexedSeq[T]]) :+ (p2: ParserT[I, T]) : ParserT[I, IndexedSeq[T]] = (for {
  ts <- p1
  t <- p2
} yield ts :+ t).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} :+ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

def [I, T](p1: ParserT[I, IndexedSeq[T]]) ++ (p2: ParserT[I, IndexedSeq[T]]) : ParserT[I, IndexedSeq[T]] = (for {
  ts1 <- p1
  ts2 <- p2
} yield ts1 ++ ts2).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} ++ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)
