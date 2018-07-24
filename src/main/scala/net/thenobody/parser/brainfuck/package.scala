package net.thenobody.parser

import cats.data._

package object brainfuck {

  sealed trait ParseError
  case class Unexpected(c: Char) extends ParseError
  case object UnexpectedEof extends ParseError
  case object Unknown extends ParseError

  sealed trait AST
  case class PtrMove(num: Int) extends AST
  case class MemMove(num: Int) extends AST
  case object Output extends AST
  case object Input extends AST
  case class Loop(sub: Seq[AST]) extends AST

  type ParserResult[A] = Either[ParseError, A]
  type Parser[A] = StateT[ParserResult, String, A]

  type BFProgram = Seq[AST]
}
