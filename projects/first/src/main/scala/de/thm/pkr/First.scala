package de.thm.pkr
import scala.util.parsing.combinator.JavaTokenParsers

sealed abstract class Exp
case class Const(n: Int) extends Exp
case class Plus(l: Exp, r: Exp) extends Exp
case class Minus(l: Exp, r: Exp) extends Exp
case class Times(l: Exp, r: Exp) extends Exp
case class Div(l: Exp, r: Exp) extends Exp
case class Call(fun: Symbol, args: List[Exp]) extends Exp

trait ExpParser extends JavaTokenParsers {

	def exp: Parser[Exp] = chainl1(term, term, addOp)
	def addOp : Parser[(Exp, Exp) ⇒ Exp] = 
	  				"+" ^^^ {(x:Exp, y: Exp) => Plus(x,y)} |
					"-" ^^^ {(x:Exp, y: Exp) => Minus(x,y)}
	def term = chainl1(factor, factor, multOp)
	def multOp : Parser[(Exp, Exp) ⇒ Exp] =
	  		"*" ^^^ {(x:Exp, y: Exp) => Times(x,y)} |
			"/" ^^^ {(x:Exp, y: Exp) => Div(x,y)}

	def factor: Parser[Exp] =
			wholeNumber ^^ { d => Const(d.toInt) } |
			"("~>exp<~")" ^^ { e => e }
}


object Main extends App with ExpParser {
	println(parseAll(exp, "-2 * (3 + 4) - 5 / 6"))
}
