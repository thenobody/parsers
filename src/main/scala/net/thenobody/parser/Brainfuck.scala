package net.thenobody.parser

object Brainfuck {

  def main(args: Array[String]): Unit = {
    println(brainfuck.Parser.parse("<>+-[asdfasdfasdf.,"))
    println(brainfuck.Parser.parse("<>+-.,"))
  }

}
