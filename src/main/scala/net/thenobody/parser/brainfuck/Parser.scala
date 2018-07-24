package net.thenobody.parser.brainfuck

import cats._
import cats.data._
import cats.syntax.all._

object Parser {

  def apply[A](fn: String => ParserResult[(String, A)])(implicit F: Applicative[ParserResult]): Parser[A] =
    StateT[ParserResult, String, A](fn)

  def runParser[A](parser: Parser[A], input: String)(implicit F: Monad[ParserResult]): ParserResult[A] =
    for {
      r <- parser.run(input)
      (_, res) = r
    } yield res

  def satisfy(fn: Char => Boolean)(implicit F: MonadError[ParserResult, ParseError]): Parser[Char] =
    Parser[Char] {
      _.toSeq match {
        case head +: tail if fn(head) => F.pure(tail.mkString, head)
        case head +: _                => F.raiseError(Unexpected(head))
        case _                        => F.raiseError(UnexpectedEof)
      }
    }

  def option[A](parser1: Parser[A], parser2: Parser[A])(implicit F: MonadError[ParserResult, ParseError]): Parser[A] =
    parser1.handleErrorWith(_ => parser2)

  def choice[A](parsers: Seq[Parser[A]])(implicit F: MonadError[ParserResult, ParseError]): Parser[A] =
    parsers.foldRight(Parser[A](_ => F.raiseError[(String, A)](Unknown)))(option[A])

  def many[A](parser: Parser[A])(implicit F: MonadError[ParserResult, ParseError]): Parser[Seq[A]] = {
    val result: Parser[Seq[A]] = for {
      a <- parser
      rest <- many(parser)
    } yield a +: rest

    result.handleError(_ => Seq())
  }

  def transform(char: Char, ast: AST)(implicit F: MonadError[ParserResult, ParseError]): Parser[AST] =
    satisfy(_ == char).map(_ => ast)

  def parseLoop(implicit F: MonadError[ParserResult, ParseError]): Parser[AST] = {
    def consume(char: Char): Parser[Unit] = satisfy(_ == char).map(_ => ())

    for {
      _ <- consume('[')
      result <- many(parseOne)
      _ <- consume(']')
    } yield Loop(result)
  }

  def parseOne(implicit F: MonadError[ParserResult, ParseError]): Parser[AST] = choice(Seq(
    transform('>', PtrMove(1)),
    transform('<', PtrMove(-1)),
    transform('+', MemMove(1)),
    transform('-', MemMove(-1)),
    transform('.', Output),
    transform(',', Input),
    parseLoop
  ))

  def eof(implicit F: MonadError[ParserResult, ParseError]): Parser[Unit] = Parser[Unit] {
    case s if s.isEmpty => F.pure(s, ())
    case _              => F.raiseError(UnexpectedEof)
  }

  def parseAll(implicit F: MonadError[ParserResult, ParseError]): Parser[BFProgram] =
    for {
      result <- many(parseOne)
      _ <- eof
    } yield result

  def parse(input: String): ParserResult[BFProgram] = {
    import cats.instances.either._
    runParser(parseAll, input.replaceAll("[^<>+\\-_.,\\[\\]]", ""))
  }

}
