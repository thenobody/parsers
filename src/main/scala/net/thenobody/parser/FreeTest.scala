package net.thenobody.parser

import cats._
import cats.instances.all._
import cats.free._

sealed trait Expr[+Next]
case class StringExpr[Next](s: String, next: Next) extends Expr[Next]

sealed trait OperatorExpr[Next] extends Expr[Next]
case class BooleanExpr[Next](op: BooleanOp, next: Next)
    extends OperatorExpr[Next]

sealed trait BooleanOp
object BooleanOp {
  case object And extends BooleanOp
  case object Or extends BooleanOp
  case object Not extends BooleanOp

  implicit val opShow: Show[BooleanOp] = Show.show {
    case And => "AND"
    case Or  => "OR"
    case Not => "NOT"
  }
}

case class ComparatorExpr[Next](comparator: Comparator, next: Next)
    extends OperatorExpr[Next]
sealed trait Comparator
object Comparator {
  case object Eq extends Comparator {
    val Value = "="
  }
  case object Lte extends Comparator {
    val Value = "<"
  }
  case object Gte extends Comparator {
    val Value = ">"
  }
  case object LteEq extends Comparator {
    val Value = "<="
  }
  case object GteEq extends Comparator {
    val Value = ">="
  }
  case object NotEq extends Comparator {
    val Value = "!="
  }

  implicit val comparatorShow: Show[Comparator] = Show.show {
    case Eq    => Eq.Value
    case Lte   => Lte.Value
    case Gte   => Gte.Value
    case LteEq => LteEq.Value
    case GteEq => GteEq.Value
    case NotEq => NotEq.Value
  }
}

case class InCollectionExpr[Next](next: Next) extends OperatorExpr[Next]

case class ParensExpr[Next](parens: ParensType, next: Next) extends Expr[Next]
sealed trait ParensType
object ParensType {
  case object ParensOpen extends ParensType {
    val Value = "("
  }
  case object ParensClose extends ParensType {
    val Value = ")"
  }

  implicit val parensShow: Show[ParensType] = Show.show {
    case ParensOpen  => ParensOpen.Value
    case ParensClose => ParensClose.Value
  }
}

case class QuoteExpr[Next](next: Next) extends Expr[Next]

object Expr {
  type ExprF[A] = Free[Expr, A]

  def pure[A](a: A): ExprF[A] = Free.pure(a)
  def stringStep(s: String): ExprF[Unit] = Free.liftF(StringExpr(s, ()))

  def andExpr: ExprF[Unit] = Free.liftF(BooleanExpr(BooleanOp.And, ()))
  def orExpr: ExprF[Unit] = Free.liftF(BooleanExpr(BooleanOp.Or, ()))
  def notExpr: ExprF[Unit] = Free.liftF(BooleanExpr(BooleanOp.Not, ()))

  def eqExpr: ExprF[Unit] = Free.liftF(ComparatorExpr(Comparator.Eq, ()))
  def lteExpr: ExprF[Unit] = Free.liftF(ComparatorExpr(Comparator.Lte, ()))
  def gteExpr: ExprF[Unit] = Free.liftF(ComparatorExpr(Comparator.Gte, ()))
  def lteEqExpr: ExprF[Unit] = Free.liftF(ComparatorExpr(Comparator.LteEq, ()))
  def gteEqExpr: ExprF[Unit] = Free.liftF(ComparatorExpr(Comparator.GteEq, ()))
  def notEqExpr: ExprF[Unit] = Free.liftF(ComparatorExpr(Comparator.NotEq, ()))
  def inCollectionExpr: ExprF[Unit] = Free.liftF(InCollectionExpr(()))

  def parensOpenExpr: ExprF[Unit] =
    Free.liftF(ParensExpr(ParensType.ParensOpen, ()))
  def parensCloseExpr: ExprF[Unit] =
    Free.liftF(ParensExpr(ParensType.ParensClose, ()))
  def quoteExpr: ExprF[Unit] = Free.liftF(QuoteExpr(()))

  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {
    def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = fa match {
      case StringExpr(s, next)        => StringExpr(s, f(next))
      case BooleanExpr(op, next)      => BooleanExpr(op, f(next))
      case ComparatorExpr(comp, next) => ComparatorExpr(comp, f(next))
      case InCollectionExpr(next)     => InCollectionExpr(f(next))
      case ParensExpr(parens, next)   => ParensExpr(parens, f(next))
      case QuoteExpr(next)            => QuoteExpr(f(next))
    }
  }

  implicit class ExprOps(self: ExprF[Unit]) {
    def -->(next: ExprF[Unit]): ExprF[Unit] = self.flatMap(_ => next)
  }
}

//sealed trait AST
//case object NilAST extends AST
//sealed trait Node[A]
//case class KeyValue(key: String, value: String, comparator: Comparator)

object FreeTest {
  def showProgram[E: Show](free: Free[Expr, E]): String = {
    def go(_free: Free[Expr, E], level: Int): String = {
      ("    " * level) + _free.fold[String](
        Show[E].show _ andThen ("return " + _ + "\n"), {
          case StringExpr(s, next) =>
            "StringExpr:" + Show[String].show(s) + "\n" + go(next, level)
          case BooleanExpr(op, next) =>
            "BooleanExpr:" + Show[BooleanOp].show(op) + "\n" + go(next, level)
          case ComparatorExpr(comparator, next) =>
            "ComparatorExpr:" + Show[Comparator]
              .show(comparator) + "\n" + go(next, level)
          case InCollectionExpr(next) =>
            "InCollectionExpr" + "\n" + go(next, level)
          case ParensExpr(parens, next) =>
            val nextLevel = parens match {
              case ParensType.ParensOpen  => level + 1
              case ParensType.ParensClose => level - 1
            }
            Show[ParensType].show(parens) + "\n" + go(next, nextLevel)
          case QuoteExpr(next) =>
            """"""" + go(next, level)
        }
      )
    }
    go(free, 0)
  }

//  def compile[A](fa: Free[Expr, A]): AST = {
//    fa.fold(NilAST)
//  }

}
