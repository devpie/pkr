package de.thm.pkr
import scala.util.parsing.combinator.JavaTokenParsers

sealed abstract class Exp1
case class Const(n: Int) extends Exp1
case class Plus(l: Exp1, r: Exp1) extends Exp1
case class Minus(l: Exp1, r: Exp1) extends Exp1
case class Times(l: Exp1, r: Exp1) extends Exp1
case class Div(l: Exp1, r: Exp1) extends Exp1
case class Call(fun: Symbol, args: List[Exp1]) extends Exp1

trait ExpParser extends JavaTokenParsers {

  def exp: Parser[Exp1] = chainl1(term, term, addOp)
  def addOp: Parser[(Exp1, Exp1) ⇒ Exp1] =
    "+" ^^^ { (x: Exp1, y: Exp1) => Plus(x, y) } |
      "-" ^^^ { (x: Exp1, y: Exp1) => Minus(x, y) }
  def term = chainl1(factor, factor, multOp)
  def multOp: Parser[(Exp1, Exp1) ⇒ Exp1] =
    "*" ^^^ { (x: Exp1, y: Exp1) => Times(x, y) } |
      "/" ^^^ { (x: Exp1, y: Exp1) => Div(x, y) }

  def factor: Parser[Exp1] =
    wholeNumber ^^ { d => Const(d.toInt) } |
      "(" ~> exp <~ ")" ^^ { e => e }
}

object Main extends App with ExpParser {
  val f = (x: Any) => (y: Any) => x
  println(f(3))
  println(f(2)(4))
  println(parseAll(exp, "-2 * (3 + 4) - 5 / 6"))

  println(a1((x) => x))

  def a1(f: Any => Any) = {
    (x: Any) => f(f(x))
  }
  
  // This is a possible Function definition without the Y-Combinator
  val fact = new Function1[Int, Int] {
    def apply(x: Int): Int = if (x == 1) x else x * apply(x - 1)
  }
}
