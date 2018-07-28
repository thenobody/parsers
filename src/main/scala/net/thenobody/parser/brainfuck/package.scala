package net.thenobody.parser

import cats._
import cats.data._
import cats.syntax.show._
import cats.instances.int._

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

  object AST {
    implicit def listShow[A](implicit show: Show[A]): Show[List[A]] = Show.show {
      list =>
        "[" + list.map(_.show).mkString(", ") + "]"
    }
    implicit def astShow: Show[AST] = Show.show {
      case PtrMove(num) => show"PtrMove($num)"
      case MemMove(num) => show"MemMove($num)"
      case Output => "Output"
      case Input => "Input"
      case Loop(sub) => show"${sub.toList}"
    }
  }

  type ParserResult[A] = Either[ParseError, A]
  type Parser[A] = StateT[ParserResult, String, A]

  type BFProgram = Seq[AST]
}
