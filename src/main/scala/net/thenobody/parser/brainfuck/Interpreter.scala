package net.thenobody.parser.brainfuck

import cats._
import cats.data._
import cats.syntax.writer._

object Interpreter {

  type Tape = (Int, Map[Int, Int])

  type Log = List[Int]
  type Res[A] = Writer[Log, A]
  type Interpreter[A] = StateT[Res, Tape, A]

  def apply[A](fn: Tape => Res[(Tape, A)])(implicit F: Applicative[Res]): Interpreter[A] = StateT[Res, Tape, A](fn)

  def memMove(i: Int)(implicit L: Monoid[Log]): Interpreter[Unit] = StateT.modify {
    case (head, tape) =>
      head -> tape.updated(head, tape.getOrElse(head, 0) + i)
  }

  def ptrMove(i: Int)(implicit L: Monoid[Log]): Interpreter[Unit] = StateT.modify {
    case (head, tape) =>
      (head + i) -> tape
  }

  def output(implicit L: Monoid[Log]): Interpreter[Unit] = StateT.modifyF {
    case (head, tape) =>
      val out = List(tape(head))
      (head, tape).writer(out)
  }

  def input(implicit L: Monoid[Log]): Interpreter[Unit] = StateT.modify {
    case (head, tape) =>
      head -> tape.updated(head, scala.io.StdIn.readInt)
  }

  def loop(sub: Interpreter[Unit])(implicit L: Monoid[Log]): Interpreter[Unit] =
    StateT.get[Res, Tape].flatMap {
      case (head, tape) if tape.get(head).exists(_ != 0) =>
        sub.flatMap(_ => loop(sub))
      case _ => StateT.pure(())
    }

  def base2(implicit L: Monoid[Log]): Interpreter[Unit] = memMove(0)

  def translate(ast: AST)(implicit L: Monoid[Log]): Interpreter[Unit] = ast match {
    case PtrMove(num) => ptrMove(num)
    case MemMove(num) => memMove(num)
    case Output       => output
    case Input        => input
    case Loop(sub)    => loop(translateAll(sub))
  }

  def translateAll(bf: BFProgram)(implicit L: Monoid[Log]): Interpreter[Unit] =
    bf.map(translate).foldLeft(base2) { case (res, in) => res.flatMap(_ => in) }

  def interpret(bf: BFProgram): Log = {
    import cats.implicits._

    translateAll(bf).runEmpty.written
  }
}
