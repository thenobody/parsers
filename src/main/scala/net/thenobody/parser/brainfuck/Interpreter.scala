package net.thenobody.parser.brainfuck

import cats.data._
import cats.implicits._

object Interpreter {

  type Tape = (Int, Map[Int, Int])

  def memMove(i: Int): State[Tape, Unit] = State.modify {
    case (head, tape) =>
      head -> tape.updated(head, tape.getOrElse(head, 0) + i)
  }

  def ptrMove(i: Int): State[Tape, Unit] = State.modify {
    case (head, tape) =>
      (head + i) -> tape
  }

  val output: State[Tape, Unit] = State.inspect {
    case (head, tape) =>
      print(tape.getOrElse(head, 0).toChar)
  }

  val input: State[Tape, Unit] = State.modify {
    case (head, tape) =>
      head -> tape.updated(head, scala.io.StdIn.readInt)
  }

  def loop(sub: State[Tape, Unit]): State[Tape, Unit] =
    State.get[Tape].flatMap {
      case (head, tape) if tape(head) > 0 =>
        sub.flatMap(_ => loop(sub))
      case _ => State.pure(())
    }

  val base: State[Tape, Unit] = memMove(0)

  def translate(ast: AST): State[Tape, Unit] = ast match {
    case PtrMove(num) => ptrMove(num)
    case MemMove(num) => memMove(num)
    case Output       => output
    case Input        => input
    case Loop(sub)    => loop(translateAll(sub))
  }

  def translateAll(bf: BFProgram): State[Tape, Unit] =
    bf.map(translate).foldLeft(base) { case (res, in) => res.flatMap(_ => in) }

  def interpret(bf: BFProgram): Unit = translateAll(bf).runEmpty.value
}
