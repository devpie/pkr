package de.thm.pkr

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader
// abstract Syntax Tree

sealed abstract class Exp
case class Variable(name: String) extends Exp
case class Abstraction(v: String, body: Exp) extends Exp
case class Application(fun: Exp, arg: Exp) extends Exp
case class Binding(b: Map[String, Int] = Map()) {
  def apply(x: String) = b.getOrElse(x, "").toString
  def +(newBinding: String) = Binding(b + Pair(newBinding, b.getOrElse(newBinding, 0)+1))
}

// Parser
object LambdaParser extends JavaTokenParsers {

  private def name = "[a-zA-Z]".r

  private def variable: Parser[Exp] = name ^^ { x => Variable(x) }

  private def term: Parser[Exp] = variable |
    ("λ" ~> name <~ ".") ~ exp ^^ { case a ~ b => Abstraction(a, b) } |
    "(" ~> exp <~ ")"

  private def exp: Parser[Exp] = rep1(term) ^^ {
    case first :: rest => (first /: rest) {
      (fun, arg) => Application(fun, arg)
    }
    case _ => throw new IllegalArgumentException("Should not occur")
  }

  def parse(s: String): Exp =
    phrase(exp)(new CharSequenceReader(s)) match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => throw new IllegalArgumentException(
        "Parser Error ’" + s + "’: " + msg)
    }
}
object Evaluator {
  def rename(e: Exp, b: Binding): Exp = 
    e match {
    case Application(e1, e2) => Application(rename(e1,b), rename(e2,b))
    case Abstraction(v, body) => Abstraction(rename(v, b + v), rename(body, b + v))
    case Variable(x) => Variable(rename(x, b))
  	}
  
  private def rename(v: String, b: Binding): String = v + b(v)
  
  private def substitute(x: String, e2: Exp, e1: Exp): Exp =
    e1 match {
      case Variable(y) if x != y => Variable(y)
      case Variable(x) => e2
      case Abstraction(y, body) =>
        Abstraction(y, substitute(x, e2, body))
      case Application(fun, arg) =>
        Application(substitute(x, e2, fun), substitute(x, e2, arg))
    }

  private def reduce(e: Exp): Exp =
    e match {
      case Application(Abstraction(v, body), arg) => substitute(v, arg, body)
      case Application(e1, e2) => Application(reduce(e1), reduce(e2))
      case _ => e
    }

  def eval(e: Exp): Exp = {
    var er = reduce(e)
    if (e == er) e else reduce(er)
  }
}

object Main extends App {
  var ok = true
  var input = ""
  while (ok) {
    input = readLine()
    ok = input != "quit"
    if (ok)
      try {
        val ast = LambdaParser.parse(input)
        println(ast)
        println(Evaluator.rename(ast,Binding()))
        println(Evaluator.eval(ast))
      } catch {
        case e: IllegalArgumentException => e.printStackTrace()
      }
  }
}
// λz.(λx.xy)uv

