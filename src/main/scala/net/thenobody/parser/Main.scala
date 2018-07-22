package net.thenobody.parser

import cats.instances.all._
import fastparse.all._
import FreeTest._
import Expr._

object Main {
  type ExprP = Parser[ExprF[Unit]]
  val stringParser: ExprP =
    P(
      CharsWhileIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "-_./?!").!
    ).map(stringStep)
  val andParser: ExprP = P(IgnoreCase("and")).!.map(_ => andExpr)
  val orParser: ExprP = P(IgnoreCase("or")).!.map(_ => orExpr)
  val notParser: ExprP = P(IgnoreCase("not")).!.map(_ => notExpr)
  val booleanOpParser: ExprP = P(andParser | orParser | notParser)

  val parensOpenParser: ExprP = P("(").map(_ => parensOpenExpr)
  val parensCloseParser: ExprP = P(")").map(_ => parensCloseExpr)
  val parensParser: ExprP = P(parensOpenParser | parensCloseParser)
  val operatorParser: ExprP =
    P(StringInIgnoreCase("=", "<", ">", "<=", ">=", "!=", "in").!).map {
      case "="  => eqExpr
      case "<"  => lteExpr
      case ">"  => gteExpr
      case "<=" => lteEqExpr
      case ">=" => gteEqExpr
      case "!=" => notEqExpr
      case "in" => inCollectionExpr
    }
  val quoteParser: ExprP = P("\"").map(_ => quoteExpr)

  def exprParser: ExprP = P(parensParser | booleanOpParser | operatorParser | quoteParser | stringParser)

  def _parser: ExprP = P(exprParser ~ (" ".rep ~ exprParser).rep).map {
    case (head, tail) => tail.fold(head)(_ --> _)
  }
  def parser: ExprP = P(_parser ~ End)

  def main(args: Array[String]): Unit = {
    val input =
      "name = \"asdsad something in and (second (fgsfds)))))"

    val result = parser.parse(input).get.value
    println(showProgram(result))
  }

}
