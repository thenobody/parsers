package net.thenobody.parser.brainfuck

import cats._
import cats.data._
import cats.syntax.show._
import cats.syntax.semigroup._
import cats.instances.int._

import scala.util.Try

object Interpreter {

  // types
  type Tape = (Int, Map[Int, Int])

  type Log = List[Int]
  type Res[A] = WriterT[Eval, Log, A]
  type Interpreter[A] = StateT[Res, Tape, A]

  implicit def interpreterSemigroup[A](
      implicit L: Monoid[Log]): Semigroup[Interpreter[A]] =
    new Semigroup[Interpreter[A]] {
      def combine(x: Interpreter[A], y: Interpreter[A]): Interpreter[A] =
        for {
          _ <- x
          res <- y
        } yield res
    }

  // formatters
  implicit val tapeCharShow: Show[Char] = Show.show {
    case c if c < 32 => show"char:${c.toInt}"
    case c           => c.toString
  }
  implicit val tapeMapShow: Show[Map[Int, Int]] = Show.show { tape =>
    tape.toList
      .sortBy(_._1)
      .map {
        case (key, value) => show"key:$key -> ${value.toChar} (int:$value)"
      }
      .mkString(", ")
  }
  implicit val tapeShow: Show[Tape] =
    Show.show {
      case (head, tape) =>
        show"head: $head; tape: $tape"
    }

  // commands
  def memMove(i: Int)(implicit L: Monoid[Log]): Interpreter[Unit] =
    for {
      value <- current
      res <- update(value + i)
    } yield res

  def ptrMove(i: Int)(implicit L: Monoid[Log]): Interpreter[Unit] =
    StateT.modify {
      case (head, tape) =>
        (head + i) -> tape
    }

  def output(implicit L: Monoid[Log],
             F: Applicative[Interpreter],
             Res: Applicative[Res]): Interpreter[Unit] = {
    for {
      value <- current
      out = List(value)
      res <- StateT.modifyF[Res, Tape] { Res.pure(_).tell(out) }
    } yield res
  }

  def read(message: Option[String])(
      implicit F: Applicative[Interpreter]): Interpreter[Int] =
    F.unit.map { _ =>
      println(message.getOrElse("provide value [Press enter]"))
      Try(scala.io.StdIn.readInt).getOrElse(0)
    }

  def current(implicit F: Applicative[Interpreter]): Interpreter[Int] =
    F.unit.inspect {
      case (head, tape) => tape.getOrElse(head, 0)
    }

  def update(value: Int)(
      implicit F: Applicative[Interpreter]): Interpreter[Unit] =
    F.unit.modify {
      case (head, tape) =>
        head -> tape.updated(head, value)
    }

  def debugPrint(ast: AST, interrupting: Boolean)(
      implicit L: Monoid[Log],
      F: Applicative[Interpreter]): Interpreter[Unit] =
    for {
      _ <- F.unit.inspect { tape =>
        Seq(
          "- debug --------------",
          show"ast: $ast",
          show"tape: $tape",
          "----------------------",
        ).foreach(println)
      }
      _ <- F.whenA(interrupting) {
        read(Some("continue? [Press enter]"))
      }
    } yield ()

  def input(implicit L: Monoid[Log]): Interpreter[Unit] =
    for {
      value <- read(None)
      res <- update(value)
    } yield res

  def loop(sub: Interpreter[Unit])(
      implicit L: Monoid[Log],
      F: Applicative[Interpreter]): Interpreter[Unit] = {
    val recurse = for {
      _ <- sub
      r <- loop(sub)
    } yield r

    for {
      value <- current
      res <- F.whenA(value != 0)(recurse)
    } yield res
  }

  def merge(sub: Seq[Interpreter[Unit]])(
      implicit L: Monoid[Log]): Interpreter[Unit] =
    sub.foldLeft(base)(_ |+| _)

  def base(implicit L: Monoid[Log]): Interpreter[Unit] = memMove(0)

  // translation
  implicit class ASTOps(ast: AST) {
    def toInterpreter(implicit L: Monoid[Log]): Interpreter[Unit] =
      ast match {
        case PtrMove(num) => ptrMove(num)
        case MemMove(num) => memMove(num)
        case Output       => output
        case Input        => input
        case Loop(sub)    => loop(merge(sub.map(_.toInterpreter)))
      }
  }

  def translate(ast: AST, debugEnabled: Boolean)(
      implicit L: Monoid[Log],
      F: Applicative[Interpreter]): Interpreter[Unit] = {
    for {
      res <- ast.toInterpreter
      _ <- F.whenA(debugEnabled)(debugPrint(ast, interrupting = true))
    } yield res
  }

  def translateAll(bf: BFProgram, debugEnabled: Boolean)(
      implicit L: Monoid[Log]): Interpreter[Unit] = {
    val translated = bf.map(translate(_, debugEnabled))
    merge(translated)
  }

  // public
  def interpret(bf: BFProgram): Log = {
    import cats.implicits._

    translateAll(bf, debugEnabled = false).runEmpty.written.value
  }

  def debug(bf: BFProgram): Log = {
    import cats.implicits._

    translateAll(bf, debugEnabled = true).runEmpty.written.value
  }
}
